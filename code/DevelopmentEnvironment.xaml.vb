Imports System.IO
Imports System.Numerics
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports System.Threading
Imports System.Windows.Media.Media3D
Imports System.Windows.Threading
Imports DevelopmentEnvironment.Base.BSPTree
Imports DevelopmentEnvironment.Scene
Imports DevelopmentEnvironment.Scene.BVH
Imports Microsoft.Win32
Imports DevelopmentEnvironment.Pipeline
Imports DevelopmentEnvironment

Public Module Constant
#If DEBUG Then
    Public Const dbg = True
#Else
    Public Const dbg = False
#End If
End Module
Module Extensions
    <Extension()>
    Function Index(ByVal v As Vector4, i As Integer) As Single
        If i = 0 Then Return v.X
        If i = 1 Then Return v.Y
        If i = 2 Then Return v.Z
        If i = 3 Then Return v.W
        Throw New Exception("Invalid Index")
    End Function

    <Extension()>
    Function Max(s1 As Single, s2 As Single) As Single
        If s1 >= s2 Then Return s1 Else Return s2
    End Function
    <Extension()>
    Function Min(s1 As Single, s2 As Single) As Single
        If s1 < s2 Then Return s1 Else Return s2
    End Function
    <Extension()>
    Function Index(ByVal v As Vector3, i As Integer) As Single
        If i = 0 Then Return v.X
        If i = 1 Then Return v.Y
        If i = 2 Then Return v.Z
        Throw New Exception("Invalid Index")
    End Function
End Module

Class DevelopmentEnvironment
    Private showFTime As Boolean = False

    'defaults
    Const defResX As Integer = 1920
    Const defResY As Integer = 1080
    Const defMove As Single = 0.5F
    Const defSense As Single = 5
    Const defFOV As Single = 90
    Const defLighting As Boolean = True
    Const defTransparency As Integer = 1
    Const defSpheresHidden As Boolean = False

    Private resX As Integer = defResX
    Private resY As Integer = defResY
    Private orig As Vector2 'used to determine scaling from the XAML editor

    'display related
    Private format As PixelFormat = PixelFormats.Pbgra32
    Private writeable As WriteableBitmap
    Dim bPtr As IntPtr
    Dim resChanged As Boolean

    'camera
    Private cam As Camera
    Private baseMove As Single = defMove
    Private baseRotate As Single = Math.PI / 2 / 2 'in radians
    Private dist As Single = 2 'distance models are placed from player

    'related to rendering
    Private renderThread As Thread 'Use another thread so that UI remains responsive
    Private frameTimeS As Single
    Private sc As New Scene

    Private pipeline As Pipeline

    'mouse variables
    Private leftClicked As Boolean
    Private cursorShown As Boolean = True
    Private sens As Single = defSense
    Private prev As Point

    'UI scaling variables (set on load)
    Private scaling As New List(Of Vector2) 'set for each viewport as a multiplier of the screen size
    Private origTransform As New List(Of Vector2)

    'fixing mouse position inaccuracies (the same amount of pixels are lost regardless of resolution, and the amount lost scales linearly across the screen)
    Private pixel_lossX As Integer = 16
    Private pixel_lossY As Integer = 41

    Private transformMode As Integer = 0 'translate=1, rotate=2, scale=3, none=0; should also use this in InputQuery
    Private prevCount As Integer = -1

    'for 3d ui
    Private desiredArea As Single = 0.05
    Private initialPos As Vector3
    Private first As Boolean
    Private initialIntersect As Vector3
    Private initialAngle As Vector3
    Private currentQ As Quaternion
    Private origRot As Vector3
    Private origSize As Vector3
    Private origUpper As Vector3
    Private origLower As Vector3

    Private cDown As Boolean = False
    Private vDown As Boolean = False
    Private zDown As Boolean = False
    Private yDown As Boolean = False

    Private preserved As Model 'when copy pasting
    Private lightDiffuse As Integer

    Public lock As New ManualResetEventSlim 'for when the bvh is being edited in parallel

    Private resources As String 'the file path to the resources folder (holding things needed by the program)
    Private fov As Single
    Private escPressed As Boolean = False

    Private rOptions As Pipeline.RenderOptions

    Private undoStack As New ActionStack(True)
    Private redoStack As New ActionStack(False)
    Private edited As Boolean = False

    Private started As Boolean = False
    Private unselect As Boolean = False

    'external input functions
    Public Declare Function GetAsyncKeyState Lib "User32.dll" (ByVal vKey As Int32) As UShort 'allows for multiple keys to be pressed at once
    Public Declare Function SetCursorPos Lib "User32.dll" (x As Integer, y As Integer) As Boolean 'allows for setting mouse position
    Public Declare Function ShowCursor Lib "User32.dll" (show As Int32) As Boolean 'allows for hiding mouse
    'note for mouse input: PointToScreen gets coordinates in terms of actual screen; without it, it is relative to window (desired for raycasting; but actual needed for setting cursor pos)

    Private Sub Load() Handles MyBase.Initialized 'before it loads and is displayed
        lock.Set() 'make sure that rendering can proceed

        'get path to resources folder from executable
        Dim base As String = Directory.GetCurrentDirectory
        Dim split() As String = base.Split("\")
        While split(split.Length - 1) <> "bin"
            base = Directory.GetParent(base).ToString
            split = base.Split("\")
        End While
        resources = Directory.GetParent(base).ToString & "\Resources"

        orig = New Vector2(Me.Width, Me.Height)
        Me.Left = 0
        Me.Top = 0
        Me.Width = resX
        Me.Height = resY

        cam = New Camera With {
            .pos = New Vector3(0, 0, 0),
            .dir = New Vector3(0, 0, 0), 'in radians, default to looking down -ve z axis
            .near = 0.1, 'represents how far the near plane is from the camera
            .far = 1000, 'represents how far the far plane is from the camera. This should be set such that any not-really-visible triangles / fragments can be culled, while maintaining depth precision
            .fov = defFOV,
            .resX = resX,
            .resY = resY}

        sc.format = format
        pipeline = New Pipeline(Environment.ProcessorCount)
        rOptions = New Pipeline.RenderOptions With {.asWireFrame = False, .toVBuffer = True, .toZBuffer = True, .lighting = defLighting, .transparency = defTransparency, .shadowRes = 512, .cullBack = 1, .hideLightSpheres = defSpheresHidden}

        'load settings in here (create a default settings config file here if it doesn't exist)
        LoadSettings()

        'set UI scaling
        'only for viewboxes, since they will correctly scale the UI within them (and the UI contents) accordingly
        CheckUI(Function(cast As Viewbox)
                    Dim newScale As New Vector2(cast.Width / orig.X, cast.Height / orig.Y)
                    scaling.Add(newScale)
                    origTransform.Add(New Vector2(cast.RenderTransform.Value.OffsetX / orig.X, cast.RenderTransform.Value.OffsetY / orig.Y))
                    Return True
                End Function)

        'start rendering
        renderThread = New Thread(Sub() RenderLoop())
        renderThread.SetApartmentState(ApartmentState.STA)
        renderThread.Start()
    End Sub

    'will also fill out the settings page according to the currently loaded settings (reset to default if there is none, or if it is not valid)
    'should perform error checking when reading to and from the xml files (if it fails, then just load defaults)
    Private Sub LoadSettings()
        If File.Exists(resources & "\settings.xml") Then 'read in settings
            XMLToSettings(baseMove, sens, fov, resX, resY, rOptions, sc.ambient)

            'fill settings in settings page
            FillSettings()

        Else 'save defaults
            ApplyDefaults()

        End If
    End Sub

    Private Sub FillSettings()
        MoveSpeed.Text = baseMove
        SensInput.Text = sens
        FovInput.Text = fov

        Res.SelectedIndex = -1
        If resY = 480 AndAlso resX = 640 Then
            Res.SelectedIndex = 0
        ElseIf resY = 720 AndAlso resX = 1280 Then
            Res.SelectedIndex = 1
        ElseIf resY = 1080 AndAlso (resX = 1920 OrElse resX = 2560) Then
            If resX = 1920 Then
                Res.SelectedIndex = 2
            Else
                Res.SelectedIndex = 3
            End If
        ElseIf resY = 1440 AndAlso (resX = 2560 OrElse resX = 3440) Then
            If resX = 2560 Then
                Res.SelectedIndex = 4
            Else
                Res.SelectedIndex = 5
            End If
        ElseIf resY = 2160 AndAlso (resX = 3840 OrElse resX = 5120) Then
            If resX = 33840 Then
                Res.SelectedIndex = 6
            Else
                Res.SelectedIndex = 7
            End If
        Else 'custom res
            Custom.Content = resX & "x" & resY & " [Custom]"
            Res.SelectedIndex = 8
        End If

        Lighting.IsChecked = rOptions.lighting
        Tr0.IsChecked = False
        Tr1.IsChecked = False
        Tr2.IsChecked = False
        If rOptions.transparency = -1 Then
            Tr0.IsChecked = True
        ElseIf rOptions.transparency = 0 Then
            Tr1.IsChecked = True
        Else
            Tr2.IsChecked = True
        End If

        LightSpheres.IsChecked = rOptions.hideLightSpheres

        Dim r255 As Single = 1.0F / 255.0F
        Dim ambient As New Vector4((sc.ambient And 255) * r255, ((sc.ambient And 65280) >> 8) * r255, ((sc.ambient And 16711680) >> 16) * r255, 1) 'in form bgra
        Red.Text = ambient.Z * 255.0F
        Green.Text = ambient.Y * 255.0F
        Blue.Text = ambient.X * 255.0F
    End Sub

    'will also update the config xml file after values in the program have been updated (when the apply button is clicked)
    Private Sub ApplySettings() Handles SettingsApply.Click
        'update values in program (start by setting temp variables to error check)
        Dim newMove As Single 'raise errors if either of these are -ve
        Dim newSense As Single
        Dim newFOV As Single 'should also raise error if the fov is 0, 180, or -ve
        Dim newResX As Integer
        Dim newResY As Integer
        Dim newLighting As Boolean
        Dim hideSpheres As Boolean
        Dim newTransparency As Integer
        Dim newAmbientR As Integer
        Dim newAmbientG As Integer
        Dim newAmbientB As Integer

        Try

            'move speed
            If Not Single.TryParse(MoveSpeed.Text, newMove) Then 'convert from text to single
                Throw New Exception("Invalid Move Speed")
            End If
            If newMove <= 0 Then 'validate input
                Throw New Exception("Invalid Move Speed")
            End If

            'sense
            If Not Single.TryParse(SensInput.Text, newSense) Then
                Throw New Exception("Invalid Sensitivity")
            End If
            If newSense <= 0 Then
                Throw New Exception("Invalid Sensitivity")
            End If

            'fov
            If Not Single.TryParse(FovInput.Text, newFOV) Then
                Throw New Exception("Invalid Field Of View")
            End If
            If newFOV >= 180 OrElse newFOV <= 0 Then
                Throw New Exception("Invalid Field Of View")
            End If

            'resolution
            Dim resolution As Label = Res.SelectedItem
            Select Case resolution.Name
                Case "Res480p"
                    newResX = 640
                    newResY = 480
                Case "Res720p"
                    newResX = 1280
                    newResY = 720
                Case "Res1080p"
                    newResX = 1920
                    newResY = 1080
                Case "Res1080pW"
                    newResX = 2560
                    newResY = 1080
                Case "Res1440p"
                    newResX = 2560
                    newResY = 1440
                Case "Res1440pW"
                    newResX = 3440
                    newResY = 1440
                Case "Res2160p"
                    newResX = 3840
                    newResY = 2160
                Case "Res2160pW"
                    newResX = 5120
                    newResY = 2160
                Case Else 'handle custom resolution here
                    Dim result() = Split(Custom.Content, "x", 2)
                    result(1) = Split(result(1), " ", 2)(0)
                    newResX = result(0)
                    newResY = result(1)
            End Select

            newLighting = Lighting.IsChecked

            If Tr0.IsChecked Then
                newTransparency = -1
            ElseIf Tr1.IsChecked Then
                newTransparency = 0
            Else
                newTransparency = 1
            End If

            hideSpheres = LightSpheres.IsChecked

            'ambience (will convert to one integer later)
            If Not Integer.TryParse(Red.Text, newAmbientR) OrElse Not Integer.TryParse(Green.Text, newAmbientG) OrElse Not Integer.TryParse(Blue.Text, newAmbientB) Then
                Throw New Exception("Invalid Ambient Settings")
            End If
            If newAmbientR < 0 OrElse newAmbientR > 255 Then
                Throw New Exception("Invalid Ambient Settings")
            ElseIf newAmbientG < 0 OrElse newAmbientG > 255 Then
                Throw New Exception("Invalid Ambient Settings")
            ElseIf newAmbientB < 0 OrElse newAmbientB > 255 Then
                Throw New Exception("Invalid Ambient Settings")
            End If

        Catch ex As Exception
            MessageBox.Show(ex.Message)
            Exit Sub
        End Try

        'set temp variables to program variables since they have been validated
        baseMove = newMove
        sens = newSense
        fov = newFOV
        resX = newResX
        resY = newResY
        rOptions.lighting = newLighting
        rOptions.hideLightSpheres = hideSpheres
        rOptions.transparency = newTransparency
        sc.ambient = BitConverter.ToInt32({newAmbientB, newAmbientG, newAmbientR, 255})

        If sc.selected IsNot Nothing AndAlso ((hideSpheres AndAlso sc.selected.lightRef <> -1) OrElse (sc.selected.contents.hasTransparent AndAlso newTransparency = -1)) Then
            sc.selected = Nothing
        End If

        Dim temp = resX 'accounting for the automatic form rescaling that changes resX to match current (see event handlers)
        Me.Height = resY
        Me.Width = temp
        resChanged = True

        SettingsToXML() 'save values to xml file
    End Sub

    'when the 'reset to default' button is clicked, will change settings page and program values and then call ApplySettings (ambience default is in scene)
    Private Sub ApplyDefaults() Handles Defaults.Click
        baseMove = defMove
        sens = defSense
        fov = defFOV
        resX = defResX
        resY = defResY
        rOptions.lighting = defLighting
        rOptions.hideLightSpheres = defSpheresHidden
        rOptions.transparency = defTransparency
        sc.ambient = sc.defAmbient

        Dim temp = resX
        Me.Height = resY
        Me.Width = temp
        resChanged = True

        'change settings page
        FillSettings()

        SettingsToXML()
    End Sub

    'writes settings to the xml config file (creates config file if one doesn't exist)
    Private Sub SettingsToXML()
        Dim r255 As Single = 1.0F / 255.0F
        Dim ambient As New Vector4((sc.ambient And 255) * r255, ((sc.ambient And 65280) >> 8) * r255, ((sc.ambient And 16711680) >> 16) * r255, 1) 'in form bgra

        Dim newSettings As XDocument = <?xml version="1.0" encoding="UTF-8"?>
                                       <Settings>
                                           <Cam>
                                               <MoveSpeed><%= baseMove %></MoveSpeed>
                                               <Sensitivity><%= sens %></Sensitivity>
                                               <FOV><%= fov %></FOV>
                                           </Cam>
                                           <Visual>
                                               <Resolution>
                                                   <X><%= resX %></X>
                                                   <Y><%= resY %></Y>
                                               </Resolution>
                                               <Lighting>
                                                   <Enabled><%= rOptions.lighting %></Enabled>
                                                   <Spheres><%= rOptions.hideLightSpheres %></Spheres>
                                                   <Ambience>
                                                       <R><%= ambient.Z * 255 %></R>
                                                       <G><%= ambient.Y * 255 %></G>
                                                       <B><%= ambient.X * 255 %></B>
                                                   </Ambience>
                                               </Lighting>
                                               <Transparency><%= rOptions.transparency %></Transparency>
                                           </Visual>
                                       </Settings>
        newSettings.Save(resources & "\settings.xml")
    End Sub

    'can pass in parameters to change by reference
    Private Sub XMLToSettings(ByRef baseMove As Single, ByRef sens As Single, ByRef fov As Single, ByRef resX As Integer, ByRef resY As Integer, ByRef rOptions As Pipeline.RenderOptions, ByRef ambient As Integer)
        Dim data = XDocument.Load(resources & "\settings.xml")
        Dim settings = data.Root

        baseMove = settings.Descendants("MoveSpeed").Value
        sens = settings.Descendants("Sensitivity").Value
        fov = settings.Descendants("FOV").Value

        resX = settings.Descendants("X").Value
        resY = settings.Descendants("Y").Value

        rOptions.lighting = settings.Descendants("Enabled").Value
        rOptions.hideLightSpheres = settings.Descendants("Spheres").Value

        sc.ambient = BitConverter.ToInt32({settings.Descendants("B").Value, settings.Descendants("G").Value, settings.Descendants("R").Value, 255})

        rOptions.transparency = settings.Descendants("Transparency").Value

        Dim temp = resX
        Me.Height = resY
        Me.Width = temp
        resChanged = True
    End Sub

    'toggles settings visibility
    Private Sub DisplaySettings()
        If SettingsView.Visibility = Visibility.Hidden Then 'display settings
            FillSettings()
            MainUI.Visibility = Visibility.Hidden
            SettingsView.Visibility = Visibility.Visible

        Else 'hide settings
            MainUI.Visibility = Visibility.Visible
            SettingsView.Visibility = Visibility.Hidden

        End If
    End Sub

    Private Sub ToggleControls() Handles ControlButton.Click
        If ControlView.Visibility = Visibility.Visible Then
            ControlView.Visibility = Visibility.Hidden
        Else
            ControlView.Visibility = Visibility.Visible
        End If
    End Sub

    Private Sub Stopping() Handles MyBase.Closed
        End
    End Sub

    Private Sub AddModel(sender As Button, e As RoutedEventArgs) Handles LoadF.Click, StartProgram.Click
        Dim thread As New Thread(Sub() sc.ChooseModel(cam, dist, Me, undoStack, redoStack, True))
        thread.Start()
    End Sub
    Private Sub SaveScene() Handles SaveS.Click
        sc.SaveScene()
    End Sub

    'this should be called and triggered to toggle transform ui (3d and 2d); guarantees toggle off if no object selected
    Private Sub EditModel() Handles EditO.Click
        If EBox.Visibility = Visibility.Visible Then 'toggle off if it is visible
            EBox.Visibility = Visibility.Hidden
            transformMode = 0
            Position3dUI()
        ElseIf sc.selected IsNot Nothing Then 'only toggle on if something is selected
            If EBox.Visibility <> Visibility.Visible Then
                EBox.Visibility = Visibility.Visible
            End If
        End If
    End Sub

    Private Sub Reset3dUI()
        Dim oldselected = sc.selected
        For i = 0 To sc.arrows.Length - 1
            sc.selected = sc.arrows(i)
            sc.EditModel(New Vector3, Tr.Translate) 'place at origin
            sc.EditModel(New Vector3, Tr.Rotate) 'rotation to 0
            sc.EditModel(New Vector3(Single.Epsilon), Tr.Scale) 'set scale to 0 (most important)
        Next
        sc.selected = oldselected
    End Sub

    'called each frame to ensure ui is positioned, scaled, and rotated correctly
    Private Sub Position3dUI()
        Reset3dUI()
        If sc.selected IsNot Nothing Then 'rotate and translate ui
            Dim oldSelected = sc.selected
            Dim tr = sc.selected.translation
            For i = 0 To sc.arrows.Length - 1
                sc.selected = sc.arrows(i)
                sc.EditModel(tr, Scene.Tr.Translate) 'translate ui
                Dim rot As New Vector3
                If i < 3 OrElse (i - 9 < 3 AndAlso i - 9 >= 0) Then 'rotate translation arrows
                    If i - 9 = 0 Then
                        rot.Y = Math.PI
                    ElseIf i = 1 OrElse i - 9 = 1 Then
                        rot.Y = -Math.Sign(i - 9) * Math.PI / 2 'using math.sign ensures opposite rotation applied for the arrows
                    ElseIf i = 2 OrElse i - 9 = 2 Then
                        rot.Z = -Math.Sign(i - 9) * Math.PI / 2
                    End If

                Else 'rotate other ui according to rotation of model (using quaternions)
                    rot = oldSelected.rot
                    Dim uirot As Pipeline.Transform
                    Pipeline.TransformMatrix(New Vector3(1), rot, New Vector3, uirot) 'get rotation matrix
                    MatrixOp.Transpose(uirot, uirot)
                    Dim uiQ = Quaternion.MatrixToQuaternion(uirot) 'convert to quaternion to calculate rotations properly
                    Dim result As Quaternion

                    If i = 3 Then 'rot
                        result = Quaternion.CreateFromAxis(1, 0, 0, 0) * uiQ
                    ElseIf i = 4 Then 'z
                        result = Quaternion.CreateFromAxis(0, 1, 0, -Math.PI / 2) * uiQ
                    ElseIf i = 5 Then 'y
                        result = Quaternion.CreateFromAxis(1, 0, 0, -Math.PI / 2) * uiQ
                    ElseIf i = 6 Then
                        result = uiQ
                    ElseIf i - 9 = 6 Then 'scale
                        result = Quaternion.CreateFromAxis(0, 0, 1, Math.PI) * uiQ
                    ElseIf i = 7 OrElse i - 9 = 7 Then
                        result = Quaternion.CreateFromAxis(0, 1, 0, -Math.Sign(i - 9) * Math.PI / 2) * uiQ
                    ElseIf i = 8 OrElse i - 9 = 8 Then
                        result = Quaternion.CreateFromAxis(0, 0, 1, -Math.Sign(i - 9) * Math.PI / 2) * uiQ
                    End If

                    uirot = Quaternion.QuaternionToMatrix(result) 'get rotation as euler angles from quaternion (-> matrix -> euler angle)
                    rot = MatrixOp.ExtractRot(uirot)
                End If
                sc.EditModel(rot, Scene.Tr.Rotate)
            Next

            Dim depthToCentre = oldSelected.depthToCentre
            Dim delta = desiredArea * depthToCentre

            If transformMode = 1 Then 'scaling ui (will not scale any if transformMode is 0)
                For i = 0 To 2
                    sc.selected = sc.arrows(i)
                    Dim tr2 = oldSelected.translation
                    sc.EditModel(New Vector3(delta), Scene.Tr.Scale)
                    If i = 0 Then 'translate ui
                        tr2.X -= sc.selected.scale.X * 1.1
                    ElseIf i = 1 Then
                        tr2.Z += sc.selected.scale.Z * 1.1
                    ElseIf i = 2 Then
                        tr2.Y += sc.selected.scale.Y * 1.1
                    End If
                    sc.EditModel(tr2, Scene.Tr.Translate)
                Next
                For i = 0 + 9 To 2 + 9
                    sc.selected = sc.arrows(i)
                    Dim tr2 = oldSelected.translation
                    sc.EditModel(New Vector3(delta), Scene.Tr.Scale)
                    If i - 9 = 0 Then
                        tr2.X += sc.selected.scale.X * 1.1
                    ElseIf i - 9 = 1 Then
                        tr2.Z -= sc.selected.scale.Z * 1.1
                    ElseIf i - 9 = 2 Then
                        tr2.Y -= sc.selected.scale.Y * 1.1
                    End If
                    sc.EditModel(tr2, Scene.Tr.Translate)
                Next
            ElseIf transformMode = 2 Then 'rot (ignore the duplicates)
                For i = 3 To 5
                    sc.selected = sc.arrows(i)
                    sc.EditModel(New Vector3(delta * 2), Scene.Tr.Scale)
                Next
            ElseIf transformMode = 3 Then
                For i = 6 To 8
                    sc.selected = sc.arrows(i)
                    sc.EditModel(New Vector3(delta), Scene.Tr.Scale)
                Next
                For i = 6 + 9 To 8 + 9
                    sc.selected = sc.arrows(i)
                    sc.EditModel(New Vector3(delta), Scene.Tr.Scale)
                Next
                For i = 6 + 9 To 8 + 9 'translate
                    sc.selected = sc.arrows(i)
                    Dim tr2 = oldSelected.translation
                    Dim amount As Single
                    Dim axis As Vector4
                    If i - 9 = 6 Then
                        'tr2.X -= sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.X * 1.1
                        amount = -sc.selected.scale.X * 1.1
                        axis = New Vector4(1, 0, 0, 1)
                    ElseIf i - 9 = 7 Then
                        'tr2.Z -= sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.Z * 1.1
                        amount = -sc.selected.scale.Z * 1.1
                        axis = New Vector4(0, 0, 1, 1)
                    ElseIf i - 9 = 8 Then
                        'tr2.Y += sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.Y * 1.1
                        amount = sc.selected.scale.Y * 1.1
                        axis = New Vector4(0, 1, 0, 1)
                    End If
                    'apply translations around local axis
                    Dim axisTransform As Pipeline.Transform
                    Pipeline.TransformMatrix(New Vector3(1), oldSelected.rot, New Vector3(0), axisTransform)
                    MatrixOp.Transpose(axisTransform, axisTransform)
                    MatrixOp.Multiply(axis, axisTransform, axis)
                    axis *= amount
                    tr2 += New Vector3(axis.X, axis.Y, axis.Z)
                    sc.EditModel(tr2, Scene.Tr.Translate)
                Next
                For i = 6 To 8 'translate
                    sc.selected = sc.arrows(i)
                    Dim tr2 = oldSelected.translation
                    Dim amount As Single
                    Dim axis As Vector4
                    If i = 6 Then
                        'tr2.X += sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.X * 1.1
                        amount = sc.selected.scale.X * 1.1
                        axis = New Vector4(1, 0, 0, 1)
                    ElseIf i = 7 Then
                        'tr2.Z += sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.Z * 1.1
                        amount = sc.selected.scale.Z * 1.1
                        axis = New Vector4(0, 0, 1, 1)
                    ElseIf i = 8 Then
                        'tr2.Y -= sc.sc.objectList(sc.selected.X)(sc.selected.Y).contents.scale.Y * 1.1
                        amount = -sc.selected.scale.Y * 1.1
                        axis = New Vector4(0, 1, 0, 1)
                    End If

                    'apply translations around local axis
                    Dim axisTransform As Pipeline.Transform
                    Pipeline.TransformMatrix(New Vector3(1), oldSelected.rot, New Vector3(0), axisTransform)
                    MatrixOp.Transpose(axisTransform, axisTransform)
                    MatrixOp.Multiply(axis, axisTransform, axis)
                    axis *= amount
                    tr2 += New Vector3(axis.X, axis.Y, axis.Z)
                    sc.EditModel(tr2, Scene.Tr.Translate)
                Next
            End If
            sc.selected = oldSelected
        End If
    End Sub

    Private Sub SelectModel()
        'ui checks here (general UI first) - don't select model if user clicked on UI
        Dim uiSelected As Boolean
        Dim pos = RelativeMousePos(Me)
        CheckUI(Function(cast As Viewbox)
                    If cast.IsMouseOver AndAlso cast.Visibility = Visibility.Visible Then 'within control
                        uiSelected = True
                        Return False 'exit early (no need to check further)
                    End If
                    Return True
                End Function)

        Dim prev = sc.selected
        If Not uiSelected Then
            sc.SelectModel(pos, cam, rOptions.hideLightSpheres) 'if 3d ui is selected, then it will set arrowSelected in sc and not unselect the current model
        End If

        'check if a model is unselected, and ensure all the edit boxes are closed
        If sc.selected Is Nothing Then
            sc.selected = prev
            unselect = True
            If EBox.Visibility = Visibility.Visible Then
                EditModel()
            End If
            ColorView.Visibility = Visibility.Hidden
        End If
    End Sub

    Private Sub TranslateModel() Handles Translate.Click
        If transformMode = 1 Then 'toggle ui
            transformMode = 0
        Else
            transformMode = 1
        End If
        Position3dUI()
    End Sub

    Private Sub RotateModel() Handles Rotate.Click
        If transformMode = 2 Then 'toggle ui
            transformMode = 0
        Else
            transformMode = 2
        End If
        Position3dUI()
    End Sub

    Private Sub ScaleModel() Handles Scale.Click
        If transformMode = 3 Then 'toggle ui
            transformMode = 0
        Else
            transformMode = 3
        End If
        Position3dUI()
    End Sub

    Public Sub Load3dUI(base As String)
        'load in transform ui (need two of each)
        For i = 0 To 1
            sc.arrows(0 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Transform\redTArrow.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(1 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Transform\blueTArrow.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(2 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Transform\greenTArrow.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(3 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Rotate\redRot.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(4 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Rotate\blueRot.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(5 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Rotate\greenRot.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(6 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Scale\redScale.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(7 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Scale\blueScale.obj", Me, False, Nothing, Nothing, Nothing)
            sc.arrows(8 + (i * 9)) = sc.LoadModel(cam, dist, base & "\Scale\greenScale.obj", Me, False, Nothing, Nothing, Nothing)
        Next
        For Each arrow In sc.arrows 'ensure flag is set
            arrow.depth0 = True
        Next
    End Sub

    'add light at user's position
    Public Sub AddLight() Handles AddL.Click
        Dim index = sc.AddLight(cam, Me)
        undoStack.Push(1, Nothing, index)
        redoStack.Clear()
    End Sub

    Public Sub EditLight() Handles EditL.Click
        If sc.selected IsNot Nothing Then
            If sc.selected.lightRef <> -1 Then
                If ColorView.Visibility = Visibility.Visible Then
                    ColorView.Visibility = Visibility.Hidden
                Else
                    ColorView.Visibility = Visibility.Visible
                End If
            Else
                ColorView.Visibility = Visibility.Hidden
            End If
        End If
    End Sub

    Public Sub ApplyColor() Handles Apply.Click
        If sc.selected IsNot Nothing Then
            If sc.selected.lightRef <> -1 Then
                Dim red As Integer
                If Integer.TryParse(R.Text, red) Then
                    Dim blue As Integer
                    If Integer.TryParse(B.Text, blue) Then
                        Dim green As Integer
                        If Integer.TryParse(G.Text, green) Then
                            If red >= 0 AndAlso red <= 255 AndAlso blue >= 0 AndAlso blue <= 255 AndAlso green >= 0 AndAlso green <= 255 Then
                                Dim color = blue Or green << 8 Or red << 16 Or 255 << 24
                                sc.lights(sc.selected.lightRef).diffuse = color
                            End If
                        End If
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub RenderLoop()
        'add items to debug if needed
        Dim fTimeLabel As New Label
        Dim rOrder As New Label

        If dbg OrElse showFTime Then
            Dispatcher.Invoke(Sub()
                                  fTimeLabel = New Label With {.Foreground = New SolidColorBrush(Color.FromRgb(255, 255, 255))}
                                  Debug.Items.Add(fTimeLabel)
                                  If dbg Then
                                      rOrder = New Label With {.Foreground = New SolidColorBrush(Color.FromRgb(255, 255, 255))}
                                      Debug.Items.Add(rOrder)
                                  End If
                              End Sub)
        End If

        'get bin directory, then the parent of that (which will store the "Resources" folder which contains the ui elements)
        Dim base As String = Directory.GetCurrentDirectory
        Dim split() As String = base.Split("\")
        While split(split.Length - 1) <> "bin"
            base = Directory.GetParent(base).ToString
            split = base.Split("\")
        End While
        base = Directory.GetParent(base).ToString & "\Resources"

        'load in transform ui (need two of each arrow)
        Load3dUI(resources)
        sc.resources = resources

        frameTimeS = 1
        While True
            sc.lock.Set() 'free scene lock
            lock.Wait() 'wait at lock here (until bvh tree changes are done)
            sc.lock.Reset() 'reset sc lock (prevent changes to bvh in parallel)

            Dim frameTime As New Stopwatch
            frameTime.Restart()

            UpdateBmp()
            If started Then InputQuery()

            cam.ambient = sc.ambient
            cam.fov = fov
            Dim models(-1) As Model
            Dim total As Integer = 0
            If dbg Then
                'draw mouse crosshair (debugging)
                Dispatcher.Invoke(Sub()
                                      Dim pos = RelativeMousePos(Me)
                                      Dim r = sc.GetMouseRay(pos, cam)
                                      pipeline.DrawLine(cam, r.origin + (r.dir * 3), r.origin + (r.dir * 5), New Pipeline.Transform, BitConverter.ToInt32({0, 0, 255, 255}), rOptions) 'red for mouse raycasts; should result in a single red dot at mouse cursor
                                  End Sub)
            End If

            'scale 3d ui and translate accordingly (only needed for translation and scale ui)
            Position3dUI()

            'manage loading bar
            Dispatcher.Invoke(Sub()
                                  If LoadBar.Value = LoadBar.Maximum Then
                                      LoadBar.Visibility = Visibility.Hidden
                                      LoadBar.Value = 0
                                      LoadBar.Maximum = 0
                                  Else
                                      LoadBar.Visibility = Visibility.Visible
                                  End If
                              End Sub)

            'iterate through the bvh tree //draw debug for bvs and models; collect models to be rendered
            Dim t As BVH.Tuple2
            If dbg Then
                Dim temp As New List(Of Model)
                t = IterateBVH(sc.sc.root, rOptions, temp) 'returns bv count in x, and model count in y
            End If

            'get total models in lists (should be same as amount rendered); used to check if the screen should be cleared or not
            For i = 0 To sc.sc.objectList.Count - 1
                total += sc.sc.objectList(i).Count
            Next

            If total > sc.arrows.Length Then 'get rid of start screen and start rendering
                If Not started Then
                    Dispatcher.Invoke(Sub()
                                          Startup.Visibility = Visibility.Hidden
                                          MainUI.Visibility = Visibility.Visible
                                      End Sub)
                    started = True
                End If
                pipeline.RenderPass(sc, cam, sc.lights.ToArray, Nothing, rOptions, writeable, bPtr, Me)
                Else 'reset screen (clear frame) if needed
                    If prevCount <> 0 Then
                    pipeline.InitialiseBuffers(cam, resX, resY, rOptions)
                    pipeline.Display(writeable, bPtr, cam, Me)
                End If
            End If

            prevCount = total

            'record frametime to ensure proper cam control
            frameTime.Stop()
            frameTimeS = frameTime.Elapsed.TotalMilliseconds

            'testing memory recollection (working)
            'GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, True, True)

            'debugging
            If dbg OrElse showFTime Then
                fTimeLabel.Dispatcher.Invoke(Sub()
                                                 If showFTime OrElse dbg Then 'show frame times if enabled
                                                     fTimeLabel.Content = Math.Round(frameTime.Elapsed.TotalMilliseconds, 2) & " ms"
                                                     If dbg Then 'only show debug view in the Debug solution configuration
                                                         rOrder.Content =
                                                            "total models: " & t.Y & "//" & total & vbCrLf &
                                                            "total bvs: " & t.X & "//" & sc.sc.bvList.Count & vbCrLf
                                                         If sc.selected IsNot Nothing Then
                                                             rOrder.Content &= "selected: " & sc.selected.index.X & ", " & sc.selected.index.Y & vbCrLf
                                                         End If
                                                         If sc.arrowSelected IsNot Nothing Then
                                                             rOrder.Content &= "ui selected: " & sc.arrowSelected.index.X & ", " & sc.arrowSelected.index.Y
                                                         End If
                                                     End If
                                                 End If
                                             End Sub) 'use invoke so that ui thread updates the control 
            End If
        End While
    End Sub

    'only for debug purposes
    Private Function IterateBVH(index As BVH.Tuple2, rOptions As Pipeline.RenderOptions, models As List(Of Model)) As BVH.Tuple2
        Dim t As New BVH.Tuple2 With {.X = 0, .Y = 0}
        If index.Y = -2 Then Return t

        Dim n1 As BVH.Tuple2
        Dim n2 As BVH.Tuple2

        If index.Y = -1 Then 'bv
            Dim current = sc.sc.bvList(index.X)
            If dbg Then pipeline.DrawLine(cam, current.upper, current.lower, New Pipeline.Transform, Integer.MaxValue, rOptions) 'debugging (bvs) //white for bounding volumes
            t.X += 1

            'continue iterating
            n1 = IterateBVH(current.contents.child1, rOptions, models)
            n2 = IterateBVH(current.contents.child2, rOptions, models)

            'calculate new totals
            t.X = t.X + n1.X + n2.X
            t.Y = t.Y + n1.Y + n2.Y

        Else 'object
            Dim current = sc.sc.objectList(index.X)(index.Y)
            models.Add(current.contents) 'add model to render
            If dbg Then
                If sc.selected IsNot Nothing AndAlso index.X = sc.selected.index.X AndAlso index.Y = sc.selected.index.Y Then 'debugging (models)
                    pipeline.DrawLine(cam, current.upper, current.lower, New Pipeline.Transform, BitConverter.ToInt32({255, 0, 0, 255}), rOptions) 'debugging //color format: bgra //blue for selected
                Else
                    pipeline.DrawLine(cam, current.upper, current.lower, New Pipeline.Transform, BitConverter.ToInt32({0, 255, 0, 255}), rOptions) 'debugging //color format: bgra //green for unselected
                End If
            End If
            t.Y += 1
        End If

        Return t
    End Function


    'records mouse click (important for low framerates, to ensure all inputs are registered => if using Mouse.LeftButtonState, it may not be still pressed by the end of the frame); also only activates when not over any UI
    Private Sub MouseLeftDown() Handles MyBase.MouseLeftButtonDown
        leftClicked = True
    End Sub
    Private Sub MouseLeftUp() Handles MyBase.MouseLeftButtonUp
        'ensure that any 3d ui will not still be selected
        sc.arrowSelected = Nothing
        initialAngle = New Vector3
        first = True
    End Sub
    Private Function RelativeMousePos(relative As IInputElement) As Point
        Dim pos = Mouse.GetPosition(relative)
        Dim cX = (pixel_lossX / resX) * pos.X 'fix mouse pos inaccuracies
        Dim cY = (pixel_lossY / resY) * pos.Y
        pos.X += cX
        pos.Y += cY
        Return pos
    End Function


    Private Sub InputQuery()
        'Ensure movement and rotation speed independant of framerate
        Dim toMove As Single = baseMove * (frameTimeS / 1000)
        Dim toRotate As Single = baseRotate * (frameTimeS / 1000)
        Dim focused As Boolean

        'check focus; handle mouse drag movements (both left and right)
        Dispatcher.Invoke(
            Sub()
                If Me.IsActive Then 'check focus
                    focused = True

                    If Mouse.RightButton = MouseButtonState.Pressed Then
                        If cursorShown Then
                            prev = PointToScreen(Mouse.GetPosition(Me))
                            cursorShown = False
                            ShowCursor(False)
                        End If

                        If Mouse.Capture(Me) Then 'captures mouse input to main window (prevents UI interaction as well until right button up)
                            Dim current As Point = PointToScreen(Mouse.GetPosition(Me)) - prev 'get change in position
                            cam.dir.X -= (current.Y / resY) * sens * (resY / resX) 'match the aspect ratio, so it feels more balanced
                            cam.dir.Y += (current.X / resX) * sens
                        End If
                        SetCursorPos(prev.X, prev.Y) 'reset mouse position

                    ElseIf Not cursorShown Then
                        cursorShown = True
                        ShowCursor(True)
                        Mouse.Capture(Nothing) 'return the mouse capture (so that UI can still also be interacted with)

                    End If

                    'check the selected model to see if it represents a light; if so, then scale should scale all dimensions and not translate; rotate should just exit early; and at the end, the light should be changed accordingly
                    If sc.arrowSelected IsNot Nothing Then 'edit model if 3d ui is selected
                        edited = True

                        Dim index As Integer 'find the axis selected
                        For i = 0 To sc.arrows.Length - 1
                            If ReferenceEquals(sc.arrowSelected, sc.arrows(i)) Then
                                index = i
                                Exit For
                            End If
                        Next
                        Dim origIndex = index
                        If index >= 9 Then index -= 9
                        Dim axisSelected = index Mod 3 '0 for x, 1 for z, 2 for y

                        'create plane (3 points default to plane across x and y)
                        Dim v41 As New Vector4(0, 0, 0, 1)
                        Dim v42 As New Vector4(1, 0, 0, 1)
                        Dim v43 As New Vector4(1, 1, 0, 1)
                        Dim n As Vector3
                        Dim pConst As Single

                        Dim q As Quaternion
                        Dim matrix As New Pipeline.Transform With { 'set up identity matrix
                        .x = New Vector4(1, 0, 0, 0),
                        .y = New Vector4(0, 1, 0, 0),
                        .z = New Vector4(0, 0, 1, 0),
                        .w = New Vector4(0, 0, 0, 1)}
                        Dim oRot As Vector3
                        Dim q1 As Quaternion

                        If transformMode = 1 Then 'tilt plane based on how far the user looks up and down (or left and right)
                            If axisSelected = 0 Then 'x
                                q = Quaternion.CreateFromAxis(1, 0, 0, cam.dir.X)
                                matrix = Quaternion.QuaternionToMatrix(q)
                            ElseIf axisSelected = 1 Then 'z
                                q = Quaternion.CreateFromAxis(0, 1, 0, Math.PI / 2) * Quaternion.CreateFromAxis(1, 0, 0, cam.dir.X) '* Quaternion.CreateFromAxis(0, 1, 0, Math.PI / 2)
                                matrix = Quaternion.QuaternionToMatrix(q)
                            ElseIf axisSelected = 2 Then 'y
                                q = Quaternion.CreateFromAxis(0, 1, 0, cam.dir.Y)
                                matrix = Quaternion.QuaternionToMatrix(q)
                            End If
                        Else

                            oRot = sc.selected.rot
                            Pipeline.TransformMatrix(New Vector3(1), oRot, New Vector3, matrix)
                            MatrixOp.Transpose(matrix, matrix)
                            q1 = Quaternion.MatrixToQuaternion(matrix) 'get quaternion representing current model rotation

                            If transformMode = 2 Then 'rot
                                If first Then 'save plane of transformation (do not change while rotating along an axis)
                                    If axisSelected = 0 Then 'x
                                        q = Quaternion.CreateFromAxis(1, 0, 0, 0) * q1 'get plane based off of model rotation (local axis)
                                        matrix = Quaternion.QuaternionToMatrix(q)
                                    ElseIf axisSelected = 1 Then 'z
                                        q = Quaternion.CreateFromAxis(0, 1, 0, -Math.PI / 2) * q1
                                        matrix = Quaternion.QuaternionToMatrix(q)
                                    ElseIf axisSelected = 2 Then 'y
                                        q = Quaternion.CreateFromAxis(1, 0, 0, -Math.PI / 2) * q1
                                        matrix = Quaternion.QuaternionToMatrix(q)
                                    End If
                                    currentQ = q
                                Else
                                    q = currentQ
                                    matrix = Quaternion.QuaternionToMatrix(q)
                                End If
                            ElseIf transformMode = 3 Then 'scale
                                'scale uses the same planes as rotation, but will tilt based on user facing direction like with translation
                                'also, the y plane will be the same initially as the x plane (unlike the rot; and the y plane will tilt differently to the x
                                If axisSelected = 0 Then 'x
                                    q = Quaternion.CreateFromAxis(1, 0, 0, cam.dir.X) * Quaternion.CreateFromAxis(1, 0, 0, 0) * q1 'get plane based off of model rotation (local axis) and cam rotation
                                    matrix = Quaternion.QuaternionToMatrix(q)
                                ElseIf axisSelected = 1 Then 'z
                                    q = Quaternion.CreateFromAxis(0, 1, 0, -Math.PI / 2) * q1
                                    matrix = Quaternion.QuaternionToMatrix(q)
                                ElseIf axisSelected = 2 Then 'y
                                    q = Quaternion.CreateFromAxis(1, 0, 0, 0) * Quaternion.CreateFromAxis(0, 1, 0, cam.dir.Y) * q1 '* Quaternion.CreateFromAxis(0, 1, 0, cam.dir.Y)
                                    matrix = Quaternion.QuaternionToMatrix(q)
                                End If

                            End If
                        End If

                        'save values for first interaction (for biasing)
                        If first Then
                            initialPos = sc.selected.translation
                        End If

                        'rotate points
                        MatrixOp.Multiply(v41, matrix, v41)
                        MatrixOp.Multiply(v42, matrix, v42)
                        MatrixOp.Multiply(v43, matrix, v43)
                        Dim v1 As New Vector3(v41.X, v41.Y, v41.Z)
                        Dim v2 As New Vector3(v42.X, v42.Y, v42.Z)
                        Dim v3 As New Vector3(v43.X, v43.Y, v43.Z)

                        'calculate normal and centre plane to model
                        n = Vector3.Normalize(Vector3.Cross(v2 - v1, v3 - v1))
                        pConst = Vector3.Dot(n, -initialPos)

                        'find intersection
                        Dim r = sc.GetMouseRay(RelativeMousePos(Me), cam)

                        Dim cont As Boolean = True
                        Dim dot As Single = Vector3.Dot(r.dir, n)
                        If dot > -Single.Epsilon AndAlso dot < Single.Epsilon Then 'parallel to plane; do not save current position or calculate further
                            cont = False
                        End If

                        If cont Then
                            Dim t As Single = -(Vector3.Dot(r.origin, n) - pConst) / dot 'Using P.N + d = 0 (get intersect)
                            Dim intersect = r.origin + (t * r.dir)

                            Dim max = 500.0F 'to prevent extremely large (and usually sudden) translations
                            If t > max OrElse t < 0 Then Exit Sub

                            'rotate intersection points (get inverse of quaternion used before); need to translate about the centre of the plane for proper rotation (and then translate back)
                            q = Quaternion.Inverse(q)
                            matrix = Quaternion.QuaternionToMatrix(q)

                            intersect += initialPos

                            Dim iV4 As New Vector4(intersect.X, intersect.Y, intersect.Z, 1)
                            MatrixOp.Multiply(iV4, matrix, iV4)
                            intersect = New Vector3(iV4.X, iV4.Y, iV4.Z)

                            intersect -= initialPos 'if it worked, intersect should have z equal to the model (since this should match the default plane)                  

                            'save values for first interaction (for biasing)
                            If first Then
                                first = False
                                origRot = sc.selected.rot
                                origSize = sc.selected.scale
                                initialIntersect = intersect
                                origUpper = sc.sc.objectList(sc.selected.index.X)(sc.selected.index.Y).upper
                                origLower = sc.sc.objectList(sc.selected.index.X)(sc.selected.index.Y).lower
                            End If

                            'perform transformations with edited intersection points
                            If transformMode = 1 Then
                                Dim sign = -1
                                If axisSelected = 0 Then 'x
                                    Dim tr = New Vector3(initialPos.X - (initialIntersect.X - intersect.X) * sign, initialPos.Y, initialPos.Z)
                                    sc.EditModel(tr, Scene.Tr.Translate) 'translate model

                                ElseIf axisSelected = 1 Then 'z
                                    Dim tr = New Vector3(initialPos.X, initialPos.Y, initialPos.Z - (initialIntersect.X - intersect.X) * sign)
                                    sc.EditModel(tr, Scene.Tr.Translate) 'translate model

                                ElseIf axisSelected = 2 Then 'y
                                    Dim tr = New Vector3(initialPos.X, initialPos.Y - (initialIntersect.Y - intersect.Y) * sign, initialPos.Z)
                                    sc.EditModel(tr, Scene.Tr.Translate) 'translate model
                                End If

                                'edit light position
                                If sc.selected.lightRef <> -1 Then
                                    sc.lights(sc.selected.lightRef).pos = -sc.selected.translation 'set position of light
                                End If

                            Else

                                If transformMode = 2 Then 'rot
                                    Dim centre = -sc.selected.translation
                                    Dim angle As Single = Math.Atan2(intersect.Y - centre.Y, intersect.X - centre.X)
                                    Dim rot As Vector3
                                    If angle < 0 Then
                                        angle += 2 * Math.PI
                                    End If

                                    'apply rotations around local axis
                                    Dim axisTransform As Pipeline.Transform
                                    Pipeline.TransformMatrix(New Vector3(1), origRot, New Vector3(0), axisTransform)
                                    MatrixOp.Transpose(axisTransform, axisTransform)
                                    Dim az As New Vector4(0, 0, 1, 1) 'representing axis
                                    Dim ax As New Vector4(1, 0, 0, 1)
                                    Dim ay As New Vector4(0, 1, 0, 1)
                                    MatrixOp.Multiply(az, axisTransform, az)
                                    MatrixOp.Multiply(ax, axisTransform, ax)
                                    MatrixOp.Multiply(ay, axisTransform, ay)

                                    If axisSelected = 0 Then 'x
                                        If initialAngle.Z = 0 Then initialAngle.Z = angle
                                        q = Quaternion.CreateFromAxis(az.X, az.Y, az.Z, initialAngle.Z - angle)
                                        initialAngle.Z = angle
                                    ElseIf axisSelected = 1 Then 'z
                                        If initialAngle.X = 0 Then initialAngle.X = angle
                                        q = Quaternion.CreateFromAxis(ax.X, ax.Y, ax.Z, initialAngle.X - angle)
                                        initialAngle.X = angle
                                    ElseIf axisSelected = 2 Then 'y
                                        If initialAngle.Y = 0 Then initialAngle.Y = angle
                                        q = Quaternion.CreateFromAxis(ay.X, ay.Y, ay.Z, -(initialAngle.Y - angle))
                                        initialAngle.Y = angle
                                    End If

                                    'apply rotation along desired axis (order defines local or global rotation)
                                    q = q1 * q
                                    matrix = Quaternion.QuaternionToMatrix(q)
                                    rot = MatrixOp.ExtractRot(matrix)
                                    sc.EditModel(rot, Scene.Tr.Rotate)

                                ElseIf transformMode = 3 Then 'scale
                                    'translate to keep object in place (needs to be same direction as mouse travel) - take difference as in translation, and take sign of that
                                    'will need to translate along the right plane (do same as rotate, with the axis transformations?)
                                    'then scale by factor
                                    'may want to restrict initialIntersect here, so that the y and z is inline with initialPos (or other axis for the other selected)

                                    'should use the initial centre point as reference
                                    Dim iDist = initialIntersect.X + initialPos.X
                                    Dim cDist = intersect.X + initialPos.X
                                    Dim dir = -1
                                    Dim trDir = -1
                                    If axisSelected = 2 Then
                                        iDist = initialIntersect.Y + initialPos.Y
                                        cDist = intersect.Y + initialPos.Y
                                    End If

                                    Dim extra = -cDist
                                    If origIndex >= 9 Then trDir = -1 : dir = 1

                                    'scaling here
                                    Dim tr As Vector3
                                    Dim scaleMod = (extra + iDist) * dir
                                    If axisSelected = 0 Then
                                        scaleMod = -scaleMod
                                    End If

                                    If sc.selected.lightRef = -1 Then 'apply normally if the model doesn't represent a light
                                        If axisSelected = 0 Then 'x
                                            tr = New Vector3(origSize.X + (extra + iDist) * -dir, origSize.Y, origSize.Z)
                                            If tr.X > 0 Then sc.EditModel(tr, Scene.Tr.Scale) Else cont = False

                                        ElseIf axisSelected = 1 Then 'z
                                            tr = New Vector3(origSize.X, origSize.Y, origSize.Z + (extra + iDist) * dir)
                                            If tr.Z > 0 Then sc.EditModel(tr, Scene.Tr.Scale) Else cont = False

                                        ElseIf axisSelected = 2 Then 'y
                                            tr = New Vector3(origSize.X, origSize.Y + (extra + iDist) * dir, origSize.Z)
                                            If tr.Y > 0 Then sc.EditModel(tr, Scene.Tr.Scale) Else cont = False
                                        End If
                                    Else 'scaling lights

                                        tr = origSize + New Vector3(scaleMod)
                                        If tr.X > 0 AndAlso tr.Y > 0 AndAlso tr.Z > 0 Then
                                            sc.EditModel(tr, Scene.Tr.Scale)
                                            Dim lightRef = sc.selected.lightRef
                                            sc.lights(lightRef).Range = tr.X 'edit the range of the light to match edit sphere
                                            'edit modifiers so the light will fill the sphere
                                            sc.lights(lightRef).linModifier = sc.defRel / tr.X
                                        End If
                                    End If

                                    Dim test = origSize - tr

                                    'apply translations around local axis (providing the model doesn't represent a light)
                                    If sc.selected.lightRef = -1 Then
                                        Dim axisTransform As Pipeline.Transform
                                        Pipeline.TransformMatrix(New Vector3(1), origRot, New Vector3(0), axisTransform)
                                        MatrixOp.Transpose(axisTransform, axisTransform)
                                        Dim ax As New Vector4(1, 0, 0, 1)
                                        Dim ay As New Vector4(0, 1, 0, 1)
                                        Dim az As New Vector4(0, 0, 1, 1)
                                        MatrixOp.Multiply(az, axisTransform, az)
                                        MatrixOp.Multiply(ax, axisTransform, ax)
                                        MatrixOp.Multiply(ay, axisTransform, ay)

                                        Dim a1 = origUpper - sc.sc.objectList(sc.selected.index.X)(sc.selected.index.Y).upper
                                        Dim a2 = sc.sc.objectList(sc.selected.index.X)(sc.selected.index.Y).lower

                                        Dim eV3 As Vector3
                                        Dim iV3 As Vector3
                                        If axisSelected = 0 Then 'x
                                            Dim eV = ax * extra
                                            Dim iV = ax * iDist
                                            eV3 = New Vector3(eV.X, eV.Y, eV.Z)
                                            iV3 = New Vector3(iV.X, iV.Y, iV.Z)
                                            tr = initialPos - (eV3 + iV3) * trDir
                                            If cont Then sc.EditModel(tr, Scene.Tr.Translate) 'translate model

                                        ElseIf axisSelected = 1 Then 'z
                                            Dim eV = az * extra
                                            Dim iV = az * iDist
                                            eV3 = New Vector3(eV.X, eV.Y, eV.Z)
                                            iV3 = New Vector3(iV.X, iV.Y, iV.Z)
                                            tr = initialPos - ((eV3 + iV3) * 0.55) * -trDir '*0.55 to account for error during testing
                                            If cont Then sc.EditModel(tr, Scene.Tr.Translate) 'translate model

                                        ElseIf axisSelected = 2 Then 'y
                                            Dim eV = ay * extra
                                            Dim iV = ay * iDist
                                            eV3 = New Vector3(eV.X, eV.Y, eV.Z)
                                            iV3 = New Vector3(iV.X, iV.Y, iV.Z)
                                            tr = initialPos - ((eV3 + iV3) * 0.5575) * trDir '*0.5575 to account for error during testing
                                            If cont Then sc.EditModel(tr, Scene.Tr.Translate) 'translate model 

                                        End If
                                    End If

                                End If
                            End If
                        End If
                    Else
                        If edited Then 'save the edit to the undo stack since the user has finished transforming
                            edited = False
                            Dim obj = sc.sc.objectList(sc.selected.index.X)(sc.selected.index.Y).contents

                            If transformMode = 1 Then
                                If initialPos <> obj.translation Then
                                    undoStack.Push(ActionStack.id.Translate, initialPos, obj)
                                    redoStack.Clear()
                                End If
                            ElseIf transformMode = 2 Then
                                If origRot <> obj.rot Then
                                    undoStack.Push(ActionStack.id.Rotation, origRot, obj)
                                    redoStack.Clear()
                                End If
                            ElseIf transformMode = 3 Then
                                If origSize <> obj.scale Then
                                    undoStack.Push(ActionStack.id.Scale, origSize, obj)
                                    redoStack.Clear()
                                End If
                            End If
                        End If
                    End If

                    If unselect Then
                        unselect = False
                        sc.selected = Nothing
                    End If

                    If leftClicked Then 'attempt to select model
                        leftClicked = False
                        SelectModel()
                    End If
                End If
            End Sub)

        If focused Then
            'handle keyboard movement here
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Escape)) AndAlso sc.arrowSelected Is Nothing Then
                If Not escPressed Then
                    Dispatcher.Invoke(Sub() DisplaySettings())
                    escPressed = True
                End If
            Else
                escPressed = False
            End If

            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Up)) Then
                cam.dir.X += toRotate
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Down)) Then
                cam.dir.X -= toRotate
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Left)) Then
                cam.dir.Y -= toRotate
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Right)) Then
                cam.dir.Y += toRotate
            End If

            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.W)) Then
                cam.pos += CamMovement(Key.W, toMove)
            End If

            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.S)) AndAlso GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftCtrl)) AndAlso sc.arrowSelected Is Nothing Then 'save scene
                sc.SaveScene()
            ElseIf GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.S)) Then
                cam.pos += CamMovement(Key.S, toMove)
            End If

            'for all of these, only activate once per key press (set a variable and reset on key up)
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.C)) AndAlso GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftCtrl)) AndAlso sc.arrowSelected Is Nothing Then 'copy model
                If sc.selected IsNot Nothing AndAlso Not cDown Then
                    cDown = True
                    Dim obj = sc.selected
                    Dim base = obj.contents
                    preserved = New Model With {.contents = base, .lightRef = obj.lightRef, .rot = obj.rot, .scale = obj.scale}
                    If obj.lightRef <> -1 Then
                        lightDiffuse = sc.lights(obj.lightRef).diffuse
                    End If

                End If
            Else
                cDown = False
            End If

            'paste a copied model (makes a second copy to preserve the original state)
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.V)) AndAlso GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftCtrl)) AndAlso sc.arrowSelected Is Nothing Then 'paste model (transform it to infront of the camera) (also select it) 
                If preserved IsNot Nothing AndAlso Not vDown Then
                    vDown = True
                    Dim base = preserved.contents
                    Dim r = sc.GetMouseRay(New Point(cam.resX / 2, cam.resY / 2), cam)
                    Dim translation = -(r.origin + (dist * Vector3.Normalize(r.dir))) 'apply intended translation (so the model is pasted in front of user)
                    Dim copy = New Model With {.contents = base}

                    Dim oldSelected = sc.selected
                    sc.selected = sc.sc.InsertLeaf(New BVH.AABB(Of Model) With {.contents = copy, .lower = copy.contents.lower, .upper = copy.contents.upper}, New BVH.Tuple2 With {.X = -1, .Y = -2})
                    sc.EditModel(translation, Scene.Tr.Translate)
                    sc.EditModel(preserved.rot, Scene.Tr.Rotate)
                    sc.EditModel(preserved.scale, Scene.Tr.Scale)

                    If preserved.lightRef <> -1 Then 'is a light editing sphere (need to edit light based off of model's current translation and scale)
                        copy.lightRef = sc.lights.Count
                        sc.lights.Add(New PointLight With {.constModifier = sc.defConst, .expModifier = 0.0F, .pos = -copy.translation, .Range = copy.scale.X, .linModifier = sc.defRel / copy.scale.X, .diffuse = lightDiffuse})
                    End If

                    undoStack.Push(ActionStack.id.Add, Nothing, copy)
                    redoStack.Clear()
                    sc.selected = oldSelected
                End If
            Else
                vDown = False
            End If

            'undo
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Z)) AndAlso GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftCtrl)) AndAlso sc.arrowSelected Is Nothing Then
                If Not zDown Then
                    zDown = True
                    UndoRedo(True)
                End If
            Else
                zDown = False
            End If

            'redo (will need to push the action onto the undo stack)
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Y)) AndAlso GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftCtrl)) AndAlso sc.arrowSelected Is Nothing Then
                If Not yDown Then
                    yDown = True
                    UndoRedo(False)
                End If
            Else
                yDown = False
            End If

            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.A)) Then
                cam.pos += CamMovement(Key.A, toMove)
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.D)) Then
                cam.pos += CamMovement(Key.D, toMove)
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Space)) Then
                cam.pos.Y += toMove
            End If
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.LeftShift)) Then
                cam.pos.Y -= toMove
            End If

            'allow resetting of position and direction
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.O)) Then
                cam.pos = New Vector3
                cam.dir = New Vector3
                cam.pos = New Vector3
            End If

            'delete model
            If GetAsyncKeyState(KeyInterop.VirtualKeyFromKey(Key.Delete)) AndAlso sc.arrowSelected Is Nothing Then
                If sc.selected IsNot Nothing Then
                    'add action to undo stack first
                    Dim range = -1
                    If sc.selected.lightRef <> -1 Then 'save range in undo stack if deleting a light sphere
                        range = sc.lights(sc.selected.lightRef).Range
                    End If
                    undoStack.Push(ActionStack.id.Delete, New Vector3(range), sc.selected)
                    redoStack.Clear()
                    sc.DeleteModel()
                End If
            End If
        End If

        'check to make sure valid camera direction (avoid the directions that cause non-invertible matrix)
        If Math.Abs(cam.dir.Y) = CSng(Math.PI / 2) OrElse Math.Abs(cam.dir.X) = CSng(Math.PI / 2) Then
            cam.dir += New Vector3(0.001)
        End If
    End Sub

    Private Sub UndoRedo(undo As Boolean)
        Dim action As Tuple(Of Model, Integer, Vector3)
        If undo Then
            action = undoStack.Pop
        Else
            action = redoStack.Pop
        End If

        Dim oldSelected = sc.selected
        If action IsNot Nothing Then
            sc.selected = action.Item1

            If action.Item2 = ActionStack.id.Add Then 'model was added
                sc.DeleteModel()
                oldSelected = Nothing
                action = New Tuple(Of Model, Integer, Vector3)(action.Item1, ActionStack.id.Delete, action.Item3) 'need the opposite for other stack
            ElseIf action.Item2 = ActionStack.id.Delete Then 'model was deleted
                Dim obj = action.Item1

                're-insert model with last transformation state saved to stack
                If obj.contents.vertex Is Nothing Then 'need to reload base
                    Dim temp As New FileParser.Buffers
                    Dim max As Vector3
                    Dim min As Vector3
                    Dim cont = FileParser.LoadFile(Me, obj.contents.filePath, temp, format, max, min)
                    If Not cont Then Exit Sub
                    obj.contents = New Base(obj.contents.filePath, sc.newID,
                                            temp.vertex, temp.index, temp.tex.texCoord,
                                            temp.tex.texVal, temp.tex.texSize, temp.tex.transparency, max, min)
                    sc.newID += 1
                End If

                Dim translation = obj.translation
                Dim rot = obj.rot
                Dim scale = obj.scale
                obj.translation = New Vector3
                obj.rot = New Vector3
                obj.scale = New Vector3

                sc.selected = sc.sc.InsertLeaf(New BVH.AABB(Of Model) With {.contents = obj, .lower = obj.contents.lower, .upper = obj.contents.upper}, New BVH.Tuple2 With {.X = -1, .Y = -2})
                sc.EditModel(translation, Scene.Tr.Translate)
                sc.EditModel(rot, Scene.Tr.Rotate)
                sc.EditModel(scale, Scene.Tr.Scale)

                If obj.lightRef <> -1 Then 'has a light (needs to reactivate light)
                    sc.lights(obj.lightRef).Range = action.Item3.X
                End If
                action = New Tuple(Of Model, Integer, Vector3)(action.Item1, ActionStack.id.Add, action.Item3) 'need the opposite for other stack
            Else 'model was transformed
                Dim previous As Vector3
                If action.Item2 = 3 Then
                    previous = action.Item1.translation
                    sc.EditModel(action.Item3, Scene.Tr.Translate)
                ElseIf action.Item2 = 4 Then
                    previous = action.Item1.rot
                    sc.EditModel(action.Item3, Scene.Tr.Rotate)
                ElseIf action.Item2 = 5 Then
                    previous = action.Item1.scale
                    sc.EditModel(action.Item3, Scene.Tr.Scale)
                End If
                action = New Tuple(Of Model, Integer, Vector3)(action.Item1, action.Item2, previous) 'since it is being added to other stack, it needs the previous vector
            End If

            'push the action onto the other stack
            If undo Then
                redoStack.Push(action.Item2, action.Item3, action.Item1)
            Else
                undoStack.Push(action.Item2, action.Item3, action.Item1)
            End If

            sc.selected = oldSelected
        End If
    End Sub

    Private Function CamMovement(input As Key, toMove As Single) As Vector3
        Dim angle As Single 'determines how much rotation is applied to transformation vector (in radians)
        Dim tVector As New Vector3(0, 0, 0)

        'default camDir.Y
        Select Case input
            Case Key.D
                angle = cam.dir.Y
            Case Key.A
                angle = cam.dir.Y - Math.PI
            Case Key.S
                angle = cam.dir.Y + (Math.PI / 2)
            Case Key.W
                angle = cam.dir.Y - (Math.PI / 2)
        End Select

        'transform vector
        tVector.X = toMove * Math.Cos(angle)
        tVector.Z = toMove * Math.Sin(angle)

        'cam.pos += tVector
        Return tVector
    End Function

    Private Sub ChangeRes() Handles MyClass.SizeChanged
        resX = Me.ActualWidth
        resY = Me.ActualHeight
        resChanged = True

        'resize ui here (scale for larger or smaller resolution)
        ResizeUI()
    End Sub
    Private Sub ResizeUI()
        Dim current As Integer = 0
        CheckUI(Function(cast)
                    'adjust size
                    cast.Width = resX * scaling(current).X
                    cast.Height = resY * scaling(current).Y

                    'adjust offsets
                    Dim old = origTransform(current)
                    cast.RenderTransform = New TranslateTransform(resX * old.X, resY * old.Y)

                    current += 1

                    Return True
                End Function)
    End Sub

    Private Sub UpdateBmp()
        Dispatcher.Invoke(
            Sub()
                If resChanged Then
                    writeable = New WriteableBitmap(resX, resY, 96, 96, format, Nothing)
                    resChanged = False
                    bPtr = writeable.BackBuffer
                    cam.resX = resX 'this is what changes the rendering resolution
                    cam.resY = resY
                End If
            End Sub)
    End Sub

    Private Sub CheckUI(act As Func(Of Viewbox, Boolean))
        For Each control As UIElement In DevelopmentEnvironment.Children
            Dim cast As Viewbox = TryCast(control, Viewbox)
            If cast IsNot Nothing Then 'run func for each viewbox
                If Not act(cast) Then Exit For 'exit if the function returns false (stop looping)
            End If
        Next
    End Sub

    'using recursive structures
    Class ActionStack
        Private current As Node

        Private depth As Integer = 0
        Private max_depth As Integer = 50 'default
        Public Property MaxDepth As Integer
            Get
                Return max_depth
            End Get
            Set(value As Integer)
                If value > 0 Then
                    max_depth = value
                End If
            End Set
        End Property

        Private recentDeleted As New List(Of Node) 'will hold 5 most recent delete actions for undo stack; when there are more, the oldest will have its base removed and then be removed itself from this list
        Private previouslyAdded As New List(Of Node) 'will hold 5 most recently undone additions to the scene for the redo stack
        Private tail As Node 'pointer to tail to remove periodically to stay within depth limits

        Private undo As Boolean

        'determines if it is a stack for undo or not
        Public Sub New(undo As Boolean)
            Me.undo = undo
        End Sub

        Private Class Node
            Public identifier As Integer '1=add/paste; 2=Delete; 3=Translate; 4=Rot; 5=Scale
            Public changed As Vector3 'what the new translate/rot/scale value is (only used if identifier is 3,4,5)

            Public target As Model
            Public nextNode As Node
            Public prevNode As Node 'useful for moving tail
        End Class

        Enum id
            null
            Add
            Delete
            Translate
            Rotation
            Scale
        End Enum

        Public Sub Push(action As Integer, transform As Vector3, model As Model)
            Dim prevTop = current
            current = New Node With {.identifier = action, .changed = transform, .target = model}
            If prevTop IsNot Nothing Then
                current.nextNode = prevTop
                prevTop.prevNode = current
            Else 'only one node (current), so tail is current
                tail = current
            End If
            depth += 1

            'check depth
            If depth > max_depth Then
                tail = tail.prevNode 'keep stack size
                tail.nextNode = Nothing
                depth -= 1
            End If

            'check if deleted, and therefore check recentDeleted
            If action = id.Delete AndAlso undo Then
                recentDeleted.Add(current)
                If recentDeleted.Count > 5 Then
                    Dim oldest = recentDeleted(0)
                    recentDeleted.RemoveAt(0)
                    oldest.target.contents = New Base With {.filePath = oldest.target.contents.filePath} 'remove all other mesh information, saving memory
                End If
            End If
            If action = id.Add AndAlso Not undo Then
                previouslyAdded.Add(current)
                If previouslyAdded.Count > 5 Then
                    Dim oldest = previouslyAdded(0)
                    previouslyAdded.RemoveAt(0)
                    oldest.target.contents = New Base With {.filePath = oldest.target.contents.filePath} 'remove all other mesh information, saving memory
                End If
            End If
        End Sub
        Public Function Pop() As Tuple(Of Model, Integer, Vector3)
            If current IsNot Nothing Then
                Dim result = current
                current = current.nextNode 'current will become nothing if there are no more items
                If current Is Nothing Then 'need to set tail to nothing too, since there are no more items
                    tail = Nothing
                ElseIf ReferenceEquals(current, tail) Then
                    tail.prevNode = Nothing 'nothing in front of the tail now
                End If
                depth -= 1

                Return New Tuple(Of Model, Integer, Vector3)(result.target, result.identifier, result.changed)
            Else
                Return Nothing
            End If
        End Function

        'useful for the redo stack, since if an action is taken with any items in redo stack, it has to be cleared
        Public Sub Clear()
            current = Nothing
            tail = Nothing
            depth = 0
            recentDeleted = New List(Of Node)
            previouslyAdded = New List(Of Node)
        End Sub
    End Class
End Class

Public Class Scene
    Public format As PixelFormat
    Public sc As New BVH 'both models and sc will have references to the same models, so references need to be managed in both when a model is removed (remove from models list and = nothing in sc)

    Public selected As Model 'will use the index held within the class to reference object list in bvh tree
    Public arrows(17) As Model 'holds the 3d ui
    Public arrowSelected As Model

    Const max As Single = 10000 'scenes are not expected to be bigger than this because of depth precision
    Public newID As Integer = 0 'starts at 0, increments, will never reuse old values
    Public lights As New List(Of PointLight)

    'ambient and ambience defaults
    Public defAmbient As Integer = BitConverter.ToInt32({128, 128, 128, 255})
    Public ambient As Integer = defAmbient

    'lighting defaults
    Public defConst As Single = 0.2F
    Public defLin As Single = 0.6F
    Public defRange As Single = 1.0F
    Public defRel As Single = 2.4F 'relationship between range and linear modifier (y = k/x)

    Public resources As String 'file path for resources

    Public lock As New ManualResetEventSlim
    'models can be effectively deleted by setting the reference in the list to nothing
    'note: unless GC.Collect is called, the memory won't be released (except for memory pressure), but it is still 'free'

    Enum Tr
        null
        Translate
        Rotate
        Scale
    End Enum

    'should allow the player to choose where to place, by allowing them to set a distance, then placing in front of them at that distanc
    'have a reference to the main form, so a loading bar can be created (bearing in mind that multiple models could be loaded at once asynchronously); also do the same when loading in the scene 
    Public Sub ChooseModel(cam As Camera, dist As Single, de As DevelopmentEnvironment, undo As DevelopmentEnvironment.ActionStack, redo As DevelopmentEnvironment.ActionStack, parallel As Boolean)
        Dim temp As New FileParser.Buffers With
            {.index = New Base.Tuple3() {},
            .tex = New FileParser.Tex With {
                .texCoord = New Vector2() {},
                .texIndices = New Vector2() {},
                .texSize = New Vector2() {},
                .texVal = New Integer()() {}}}

        'get file and model details
        Dim fileDialog As New OpenFileDialog
        Dim supportedList As String = ""

        'Gets all of the supported file types and automatically formats them for filter
        For i = 0 To FileParser.supported.Length - 1
            supportedList &= "*" & FileParser.supported(i)

            If i <> FileParser.supported.Length - 1 Then
                supportedList &= ";"
            End If
        Next

        fileDialog.Title = "Select File"
        fileDialog.Filter = "Supported (" & supportedList & ")|" & supportedList & "|All Files (*.*)|*.*"
        fileDialog.RestoreDirectory = True
        fileDialog.ShowDialog()

        'ensure that focus is returned to the main form after the dialog is closed
        de.Dispatcher.Invoke(Sub()
                                 de.Focus()
                             End Sub)
        Dim newModel = LoadModel(cam, dist, fileDialog.FileName, de, parallel, Nothing, undo, redo)
        If newModel IsNot Nothing Then
            undo.Push(DevelopmentEnvironment.ActionStack.id.Add, Nothing, newModel) 'only push if a model is loaded, and if it is not a .scn file loaded in
            redo.Clear()
        End If
    End Sub

    Public Function LoadModel(cam As Camera, dist As Single, fileName As String, de As DevelopmentEnvironment, parallel As Boolean, optModel As Model, undo As DevelopmentEnvironment.ActionStack, redo As DevelopmentEnvironment.ActionStack) As Model
        Dim temp As New FileParser.Buffers With
            {.index = New Base.Tuple3() {},
            .tex = New FileParser.Tex With {
                .texCoord = New Vector2() {},
                .texIndices = New Vector2() {},
                .texSize = New Vector2() {},
                .texVal = New Integer()() {}}}

        If fileName <> "" Then
            Dim ext = Split(fileName, ".")
            Dim e = ext(ext.Length - 1)
            If e = "scn" Then
                Dim models(-1)() As BVH.AABB(Of Model)
                Dim bvs(-1) As BVH.AABB(Of BVH.BV)
                Dim lights(-1) As PointLight
                Dim newRoot As BVH.Tuple2
                Dim cont = FileParser.LoadSCN(de, fileName, format, Me, models, bvs, lights, ambient, newRoot)

                'may want to use locks to stop rendering while the bvh is being replaced or reset (only do if being called by chooseModel which runs in parallel)
                If parallel Then
                    de.lock.Reset()
                    lock.Wait()
                End If

                'clear stacks if a scene is loaded
                If undo IsNot Nothing Then
                    undo.Clear()
                End If
                If redo IsNot Nothing Then
                    redo.Clear()
                End If

                arrowSelected = Nothing
                selected = Nothing
                If cont Then
                    'convert the arrays into lists
                    sc.objectList = New List(Of List(Of BVH.AABB(Of Model)))
                    Dim currentArrow As Integer = 0

                    For i = 0 To models.Length - 1
                        sc.objectList.Add(New List(Of BVH.AABB(Of Model)))
                        sc.objectList(i).AddRange(models(i))
                        For n = 0 To models(i).Length - 1
                            If models(i)(n).contents.depth0 Then 'set arrows to new ones
                                arrows(currentArrow) = models(i)(n).contents
                                If currentArrow < 9 Then
                                    currentArrow += 9
                                Else
                                    currentArrow -= 8
                                End If
                            End If
                            models(i)(n).contents.index = New BVH.Tuple2 With {.X = i, .Y = n} 'ensure references are set for models upon load in
                        Next
                    Next


                    sc.root = newRoot
                    sc.bvList = New List(Of BVH.AABB(Of BVH.BV))
                    sc.bvList.AddRange(bvs)

                    Me.lights = New List(Of PointLight)
                    Me.lights.AddRange(lights)

                Else 'need to load in transform ui after resetting scene data (since it didn't load correctly)
                    sc = New BVH
                    Me.lights = New List(Of PointLight)
                    newID = 0

                    ReDim arrows(arrows.Length - 1) 'reset array (do not preserve)
                    de.Load3dUI(resources)
                End If
                If parallel Then de.lock.Set()
                Return Nothing
            Else

                'should avoid adding duplicate paths
                Dim duplicate As Boolean = False
                Dim index As Integer
                For i = 0 To sc.objectList.Count - 1
                    If sc.objectList(i).Count > 0 AndAlso sc.objectList(i)(0).contents IsNot Nothing AndAlso sc.objectList(i)(0).contents.contents.filePath = fileName Then 'model already exists
                        index = i
                        duplicate = True
                    End If
                Next

                If Not duplicate Then
                    Dim max As Vector3
                    Dim min As Vector3

                    Dim cont = FileParser.LoadFile(de, fileName, temp, format, max, min)
                    If Not cont Then Return Nothing

                    Dim base As New Base(fileName, newID,
                     temp.vertex, temp.index, temp.tex.texCoord,
                     temp.tex.texVal, temp.tex.texSize, temp.tex.transparency, max, min)
                    newID += 1

                    Dim newModel As Model
                    If optModel Is Nothing Then
                        newModel = New Model With {.contents = base}
                    Else 'add pre-existing model to scene with the desired base
                        newModel = optModel
                        newModel.contents = base
                    End If

                    'space model according to player choice
                    Dim r = GetMouseRay(New Point(cam.resX / 2, cam.resY / 2), cam) 'get ray from centre of screen
                    newModel.translation = -(r.origin + (dist * Vector3.Normalize(r.dir)))

                    If parallel Then 'ensure that rendering has stopped when the tree is updated
                        de.lock.Reset()
                        lock.Wait()
                    End If

                    'remember to adjust boundaries (only translation for adding models)
                    Dim result = sc.InsertLeaf(New BVH.AABB(Of Model) With {.contents = newModel, .lower = newModel.contents.lower - newModel.translation, .upper = newModel.contents.upper - newModel.translation}, New BVH.Tuple2 With {.X = -1, .Y = -2})
                    If parallel Then de.lock.Set() 'allow rendering
                    Return result
                Else
                    Dim newModel As Model
                    If optModel Is Nothing Then
                        newModel = New Model With {.contents = sc.objectList(index)(0).contents.contents}
                    Else 'add pre-existing model to scene with the desired base
                        newModel = optModel
                        newModel.contents = sc.objectList(index)(0).contents.contents
                    End If

                    'space model according to player choice
                    Dim r = GetMouseRay(New Point(cam.resX / 2, cam.resY / 2), cam) 'get ray from centre of screen
                    newModel.translation = -(r.origin + (dist * Vector3.Normalize(r.dir)))

                    If parallel Then
                        de.lock.Reset()
                        lock.Wait()
                    End If

                    'remember to adjust boundaries (only translation for adding models)
                    Dim result = sc.InsertLeaf(New BVH.AABB(Of Model) With {.contents = newModel, .lower = newModel.contents.lower - newModel.translation, .upper = newModel.contents.upper - newModel.translation}, New BVH.Tuple2 With {.X = -1, .Y = -2})
                    If parallel Then de.lock.Set()
                    Return result
                End If
            End If
        End If
        Return Nothing
    End Function

    Public Sub SaveScene()
        'use a dialog to get directory and file name
        Dim fileDialog As New SaveFileDialog With {
            .Title = "Select File",
            .Filter = "SCN|*.scn|All Files (*.*)|*.*",
            .RestoreDirectory = True
        }
        fileDialog.ShowDialog()

        'view the file format specification (.SCN File) for more details
        If fileDialog.FileName <> "" Then
            Dim s = System.IO.File.Open(fileDialog.FileName, FileMode.Create)
            Dim file As New BinaryWriter(s, Text.Encoding.UTF8)

            file.Write("<Bases>") 'saving base model information
            file.Write(sc.objectList.Count) 'amount expected    
            For Each base In sc.objectList
                If base.Count > 0 Then 'for each base list in the scene bvh tree
                    file.Write(base(0).contents.contents.filePath)

                    file.Write(base(0).contents.contents.upper.X) 'write other base information
                    file.Write(base(0).contents.contents.upper.Y)
                    file.Write(base(0).contents.contents.upper.Z)
                    file.Write(base(0).contents.contents.lower.X)
                    file.Write(base(0).contents.contents.lower.Y)
                    file.Write(base(0).contents.contents.lower.Z)
                End If
            Next

            Dim total As Integer 'get total models expected
            For Each base In sc.objectList
                For Each model In base
                    total += 1
                Next
            Next
            file.Write("<Models>")
            file.Write(total)

            For Each base In sc.objectList
                Dim count = 0
                For i = 0 To base.Count - 1 'count amount of models (that aren't nothing => deleted)
                    If base(i).contents IsNot Nothing Then count += 1
                Next
                file.Write(count) 'amount of models expected for this base

                For i = 0 To base.Count - 1
                    Dim model = base(i).contents 'saving individual model information                
                    If model Is Nothing Then Continue For

                    file.Write(base(i).parent) 'aabb information
                    file.Write(base(i).upper.X)
                    file.Write(base(i).upper.Y)
                    file.Write(base(i).upper.Z)
                    file.Write(base(i).lower.X)
                    file.Write(base(i).lower.Y)
                    file.Write(base(i).lower.Z)

                    file.Write(model.translation.X) 'save transformation information
                    file.Write(model.translation.Y)
                    file.Write(model.translation.Z)
                    file.Write(model.rot.X)
                    file.Write(model.rot.Y)
                    file.Write(model.rot.Z)
                    file.Write(model.scale.X)
                    file.Write(model.scale.Y)
                    file.Write(model.scale.Z)

                    file.Write(model.depth0) 'save extra information
                    file.Write(model.lightRef)
                Next
            Next

            file.Write("<BVs>")
            file.Write(sc.bvList.Count)
            file.Write(sc.root.X) 'write root for bv
            file.Write(sc.root.Y)
            For Each bv In sc.bvList
                file.Write(bv.parent) 'start by writing AABB details

                file.Write(bv.upper.X)
                file.Write(bv.upper.Y)
                file.Write(bv.upper.Z)
                file.Write(bv.lower.X)
                file.Write(bv.lower.Y)
                file.Write(bv.lower.Z)

                file.Write(bv.contents.child1.X) 'write bv references to children
                file.Write(bv.contents.child1.Y)
                file.Write(bv.contents.child2.X)
                file.Write(bv.contents.child2.Y)
            Next

            file.Write("<Lighting>")
            file.Write(lights.Count)
            For Each light In lights
                file.Write(light.pos.X)
                file.Write(light.pos.Y)
                file.Write(light.pos.Z)
                file.Write(light.diffuse)
                file.Write(light.Range)
                file.Write(light.constModifier)
                file.Write(light.linModifier)
                file.Write(light.expModifier)
            Next
            file.Write(ambient)

            file.Close()
            s.Close()
        End If
    End Sub


    'mPos in terms of screen resolution
    Public Sub SelectModel(mPos As Point, cam As Camera, lightSpheresHidden As Boolean)
        Dim r = GetMouseRay(mPos, cam)
        sc.CheckIntersect(r, selected, arrowSelected, lightSpheresHidden)
    End Sub

    Public Function GetMouseRay(mPos As Point, cam As Camera) As Ray3
        'raycast from where cursor is (ie. straight line between cam and cursor when cursor is transformed back into 3d, and the ray is extended)
        Dim pos As New Vector2(mPos.X / cam.resX, mPos.Y / cam.resY) 'normalise to (0,0) top left; (1,1) bottom right
        pos = (pos * 2) - New Vector2(1) 'normalise such that (-1,-1) top left; (0,0) middle, (1,1) bottom right

        'change where -ve x is left, +ve y is up; depth coordinate of mouse will always be 0.1
        Dim change As New Vector2(pos.X * cam.near, pos.Y * cam.near) 'change in direction from cam

        Dim tr1 As New Pipeline.Transform
        Dim tr2 As New Pipeline.Transform
        Dim fov = cam.fov * (Math.PI / 180)

        Pipeline.TransformMatrix(New Vector3(1), New Vector3(cam.dir.X, -cam.dir.Y, cam.dir.Z), New Vector3, tr2) 'apply cam rotation
        MatrixOp.Transpose(tr2, tr2)
        Dim dir4 As New Vector4(change.X * Math.Tan(fov / 2), -change.Y * (cam.resY / cam.resX) * Math.Tan(fov / 2), -cam.near, 1) 'adjusting the initial x and y has the desired effect, ensuring they are scaled appropriately (fov and aspect ratio)
        MatrixOp.Multiply(dir4, tr2, dir4)

        'the position of the ray when displayed is slightly inaccurate (because of wpf though, mouse inputs are not very accurate here)
        Return New Ray3 With {.origin = cam.pos, .dir = New Vector3(dir4.X, dir4.Y, dir4.Z), .min = 0, .max = max}
    End Function


    Public Sub EditModel(transform As Vector3, mode As Integer)
        'will edit model, create a copy of the aabb, then remove and re-add (to ensure balancing)
        If selected IsNot Nothing AndAlso mode <> 0 Then
            Dim old As Vector3
            Dim chosen = selected
            If mode = Tr.Translate Then
                old = chosen.translation
                chosen.translation = transform
            ElseIf mode = Tr.Rotate Then
                old = chosen.rot
                chosen.rot = transform
            ElseIf mode = Tr.Scale Then
                old = chosen.scale
                chosen.scale = transform
            End If

            'make changes to bvh tree (make changes to object bounding box and then call Refitting on its parent); rotation will require more work
            'will be making changes from the reference upper & lower held in the base class
            If transform.X <> old.X OrElse transform.Y <> old.Y OrElse transform.Z <> old.Z Then 'only make changes if the transformation has changed
                Dim tr As New Pipeline.Transform
                Pipeline.TransformMatrix(chosen.scale, chosen.rot, chosen.translation, tr)
                MatrixOp.Transpose(tr, tr)

                'update aabb and then refit
                Dim aabb = sc.objectList(selected.index.X)(selected.index.Y)

                'need to transform all 8 corners because need to be able to apply rotation after any rotational changes have been made
                Dim v1 = New Vector4(chosen.contents.upper, 1) 'gets the original upper and lower boundaries (may need to set this in addmodel subroutine in scene, for transformations (since this may not include that?)
                Dim v2 = New Vector4(chosen.contents.lower, 1)
                Dim v3 = New Vector4(v1.X, v1.Y, v2.Z, 1)
                Dim v4 = New Vector4(v1.X, v2.Y, v1.Z, 1)
                Dim v5 = New Vector4(v1.X, v2.Y, v2.Z, 1)
                Dim v6 = New Vector4(v2.X, v1.Y, v1.Z, 1)
                Dim v7 = New Vector4(v2.X, v1.Y, v2.Z, 1)
                Dim v8 = New Vector4(v2.X, v2.Y, v1.Z, 1)
                MatrixOp.Multiply(v1, tr, v1)
                MatrixOp.Multiply(v2, tr, v2)
                MatrixOp.Multiply(v3, tr, v3)
                MatrixOp.Multiply(v4, tr, v4)
                MatrixOp.Multiply(v5, tr, v5)
                MatrixOp.Multiply(v6, tr, v6)
                MatrixOp.Multiply(v7, tr, v7)
                MatrixOp.Multiply(v8, tr, v8)

                Dim mn As New Vector3(v8.X.Min(v7.X.Min(v6.X.Min(v5.X.Min(v4.X.Min(v3.X.Min(v2.X.Min(v1.X))))))), v8.Y.Min(v7.Y.Min(v6.Y.Min(v5.Y.Min(v4.Y.Min(v3.Y.Min(v2.Y.Min(v1.Y))))))), v8.Z.Min(v7.Z.Min(v6.Z.Min(v5.Z.Min(v4.Z.Min(v3.Z.Min(v2.Z.Min(v1.Z))))))))
                Dim mx As New Vector3(v8.X.Max(v7.X.Max(v6.X.Max(v5.X.Max(v4.X.Max(v3.X.Max(v2.X.Max(v1.X))))))), v8.Y.Max(v7.Y.Max(v6.Y.Max(v5.Y.Max(v4.Y.Max(v3.Y.Max(v2.Y.Max(v1.Y))))))), v8.Z.Max(v7.Z.Max(v6.Z.Max(v5.Z.Max(v4.Z.Max(v3.Z.Max(v2.Z.Max(v1.Z))))))))

                aabb.upper = mx
                aabb.lower = mn

                sc.RemoveLeaf(selected.index, True)
                selected = sc.InsertLeaf(aabb, selected.index)
            End If
        End If
    End Sub

    Public Sub DeleteModel()
        If selected IsNot Nothing Then
            If selected.lightRef <> -1 Then
                lights(selected.lightRef).Range = 0 'effectively remove the light referenced by model
            End If
            sc.RemoveLeaf(selected.index, False)
            selected = Nothing
        End If
    End Sub

    'will add light to the list, as well as link it to a transparent model that can be interacted with to edit the light
    Public Function AddLight(cam As Camera, de As DevelopmentEnvironment) As Model
        Dim index = lights.Count
        lights.Add(New PointLight With {.pos = cam.pos, .Range = defRange, .constModifier = defConst, .linModifier = defRel, .expModifier = 0.0F}) 'add light with default values at cam pos
        selected = LoadModel(cam, 0, resources & "\EditSphere\EditSphere.obj", de, False, Nothing, Nothing, Nothing)
        selected.lightRef = index
        EditModel(New Vector3(lights(index).Range), Scene.Tr.Scale) 'scale so the sphere represents range of light (when scaling the sphere, will need to adjust modifiers (inversely proportional to range) so that light is basically 0 at edge of range)
        Return selected
    End Function


    Public Class BVH
        Public Structure AABB(Of T)
            Public parent As Integer 'need parent reference here so that the tree can be traversed from both BVs and models; parent always references bv list

            Public upper As Vector3
            Public lower As Vector3

            Public contents As T
        End Structure


        Public Structure BV
            Public child1 As Tuple2
            Public child2 As Tuple2
        End Structure

        'used for cost calculations when picking a sibling for leaf insertion
        Public Structure Cost
            Public contents As Tuple2
            Public cost As Single 'cost for current node

            Public prev As Single 'change in SA for all previous nodes (including parent)
        End Structure

        Public Structure Tuple2
            Public X As Integer
            Public Y As Integer '-1 = bv; -2 = no child; >= 0 references object
        End Structure

        Public bvList As New List(Of AABB(Of BV))
        Public objectList As New List(Of List(Of AABB(Of Model))) 'used this way to stop re-loading in duplicate models from file
        Public root As Tuple2 'start of tree (referencing the bvList)

        Public objCount As Integer = 0

        Public Sub New()
            'important to set upper and lower bounds to min and max respectively, by default
            Dim empty As New Tuple2 With {.X = -1, .Y = -2}
            Dim start As New AABB(Of BV) With
                {.parent = -1, .upper = New Vector3(Single.MinValue), .lower = New Vector3(Single.MaxValue), .contents = New BV With {
                .child1 = New Tuple2 With {.X = -1, .Y = -2}, .child2 = New Tuple2 With {.X = -1, .Y = -2}}}
            root = New Tuple2 With {.X = 0, .Y = -1}
            bvList.Add(start)
        End Sub

        'CheckLists will automatically try to fill in gaps in the bv list
        Public Sub CheckLists(index As Tuple2)
            If index.Y = -1 AndAlso bvList.Count - 1 = root.X Then 'manage start node
                root.X = index.X
            End If

            If index.Y = -1 Then 'bv
                'ensure index is not last in list (then just remove from list and return immediately)
                Dim last = bvList(bvList.Count - 1)
                If index.X = bvList.Count - 1 Then
                    bvList.RemoveAt(bvList.Count - 1)
                Else
                    bvList(index.X) = last 'set in gap

                    'set child references
                    Dim c1 = last.contents.child1
                    Dim c2 = last.contents.child2
                    If c1.Y = -1 Then 'is bv
                        Dim c = bvList(c1.X)
                        c.parent = index.X
                        bvList(c1.X) = c
                    Else
                        Dim c = objectList(c1.X)(c1.Y)
                        c.parent = index.X
                        objectList(c1.X)(c1.Y) = c
                    End If
                    If c2.Y = -1 Then 'is bv
                        Dim c = bvList(c2.X)
                        c.parent = index.X
                        bvList(c2.X) = c
                    Else
                        Dim c = objectList(c2.X)(c2.Y)
                        c.parent = index.X
                        objectList(c2.X)(c2.Y) = c
                    End If

                    'set parent references
                    Dim p = last.parent
                    If p <> -1 Then 'not start node
                        Dim parent = bvList(last.parent)
                        If parent.contents.child1.X = bvList.Count - 1 AndAlso parent.contents.child1.Y = -1 Then 'need to change child1
                            parent.contents.child1.X = index.X
                        Else 'need to change child 2
                            parent.contents.child2.X = index.X
                        End If
                        bvList(last.parent) = parent
                    End If
                    bvList.RemoveAt(bvList.Count - 1)
                End If
            End If
        End Sub

        'for both inserting and removing, the moving index determines if it is being moved (so referencing where it still is in the list; only updating the AABB and not adding a new one
        'if the index Y is -2 (ie. adding a new object, not moving), then it will be treated as such
        Public Function InsertLeaf(obj As AABB(Of Model), moving As Tuple2) As Model
            Dim leafIndex As Tuple2
            objCount += 1

            If moving.Y = -2 Then
                'determine where the leaf should be added
                Dim base As Integer = -1
                For i = 0 To objectList.Count - 1
                    If objectList(i).Count > 0 AndAlso objectList(i)(0).contents IsNot Nothing AndAlso objectList(i)(0).contents.contents.filePath = obj.contents.contents.filePath Then 'base model already loaded
                        base = i
                    End If
                Next

                'add object to list
                If base = -1 Then 'first of its base
                    objectList.Add(New List(Of AABB(Of Model)) From {obj})
                    leafIndex = New Tuple2 With {.X = objectList.Count - 1, .Y = 0}
                Else
                    objectList(base).Add(obj)
                    leafIndex = New Tuple2 With {.X = base, .Y = objectList(base).Count - 1}
                End If

            Else 'just update the AABB
                leafIndex = moving
                objectList(moving.X)(moving.Y) = obj
            End If

            'add to tree
            Dim sibling As Tuple2 = Pick(obj)
            Dim parent = InsertParent(sibling, leafIndex)
            If sibling.Y = -1 Then Refitting(sibling.X) Else Refitting(parent.X)

            obj.contents.index = leafIndex
            Return obj.contents
        End Function


        'removes the object at that index (always expecting one from objectList)
        Public Sub RemoveLeaf(index As Tuple2, moving As Boolean)
            'Get sibling, remove self and parent bv, set grandparent's new child to sibling, and then refit
            'If the parent is start node, then only remove self and set start node's child reference to -1 (unless start has a child bv, in which case, remove start node
            'If moving is false, then do not actually remove the model from the object list, just remove the references (and bv if needed) (so perform this check at the end)
            objCount -= 1

            Dim parent As Integer = objectList(index.X)(index.Y).parent
            Dim sibling As Tuple2
            Dim gParent As Integer = bvList(parent).parent

            Dim bvRemoved As New Tuple2 With {.X = -1, .Y = -2} 'default Y = -2 for no list check

            If parent = root.X Then 'is start node
                Dim change = bvList(parent)

                'remove child reference
                If change.contents.child1.X = index.X AndAlso change.contents.child1.Y = index.Y Then 'remove child 1
                    change.contents.child1 = New Tuple2 With {.X = -1, .Y = -2}

                    If change.contents.child2.Y = -1 Then 'if other child is bv, change root and delete start node
                        root = change.contents.child2
                    End If
                Else 'remove child 2
                    change.contents.child2 = New Tuple2 With {.X = -1, .Y = -2}

                    If change.contents.child1.Y = -1 Then 'if other child is bv, change root and delete start node
                        root = change.contents.child1
                    End If
                End If

                If parent <> root.X Then 'root has been changed
                    Dim c = bvList(root.X)
                    c.parent = -1 'ensure parent is set to -1 for new root (for correct refitting)
                    bvList(root.X) = c

                    change = Nothing 'effectively delete start node
                    bvRemoved = New Tuple2 With {.X = parent, .Y = -1} 'mark for later
                End If
                bvList(parent) = change
                gParent = root.X 'enable correct refitting (otherwise gParent would be -1)

            Else 'is not start node
                'remove the parent with only one child, and move that child to the grandparent
                If bvList(parent).contents.child1.X = index.X AndAlso bvList(parent).contents.child1.Y = index.Y Then 'get said child from parent
                    sibling = bvList(parent).contents.child2
                Else
                    sibling = bvList(parent).contents.child1
                End If

                'set sibling's parent to grandparent
                If sibling.Y = -1 Then 'is bv
                    Dim s = bvList(sibling.X)
                    s.parent = gParent
                    bvList(sibling.X) = s
                Else
                    Dim s = objectList(sibling.X)(sibling.Y)
                    s.parent = gParent
                    objectList(sibling.X)(sibling.Y) = s
                End If

                bvList(parent) = Nothing
                bvRemoved = New Tuple2 With {.X = parent, .Y = -1}

                'set grandparent's child
                Dim change = bvList(gParent)
                If bvList(gParent).contents.child1.X = parent AndAlso bvList(gParent).contents.child1.Y = -1 Then 'find the gParent's child that is parent
                    change.contents.child1 = sibling 'set grandparent's child to the other remaining sibling
                Else
                    change.contents.child2 = sibling
                End If
                bvList(gParent) = change
            End If

            If Not moving Then
                Dim model = objectList(index.X)(index.Y).contents
                objectList(index.X)(index.Y) = Nothing 'remove model from list
            End If

            Refitting(gParent) 'refit before index changes are made in CheckLists (otherwise, gParent may point to the wrong thing, if it is at end of list)
            If Not moving Then CheckLists(index) 'model only removed if not moving
            If bvRemoved.Y <> -2 Then 'a bv is actually removed
                CheckLists(bvRemoved)
            End If
        End Sub


        'inserts a parent for the selected sibling and the new leaf; returns index of the new parent
        Private Function InsertParent(sibling As Tuple2, leafIndex As Tuple2) As Tuple2
            'start by checking if the sibling is a bv, and if it is missing children (in which case, just add straight to sibling)
            'otherwise, create a new parent and insert between current parent and sibling
            'will return the parent to begin resizing from

            Dim missing As Integer = 0 '-1 = left, 0 = none missing, 1 = right
            Dim oldParent As Integer 'will need old parent if adding new parent
            Dim bv As Boolean = False
            Dim obj = objectList(leafIndex.X)(leafIndex.Y)
            Dim newParent As Tuple2

            'check for missing children
            If sibling.Y = -1 Then 'sibling is bv
                bv = True
                oldParent = bvList(sibling.X).parent

                If bvList(sibling.X).contents.child1.Y = -2 Then 'no child here
                    missing = -1
                ElseIf bvList(sibling.X).contents.child2.Y = -2 Then
                    missing = 1
                End If
            Else
                oldParent = objectList(sibling.X)(sibling.Y).parent
            End If

            'either: add object, or create new parent and add object
            If missing = -1 Then
                Dim s = bvList(sibling.X)
                s.contents.child1 = leafIndex
                bvList(sibling.X) = s
                Return sibling

            ElseIf missing = 1 Then
                Dim s = bvList(sibling.X)
                s.contents.child2 = leafIndex
                bvList(sibling.X) = s
                Return sibling

            Else
                bvList.Add(New AABB(Of BV) With {
                           .parent = oldParent, .contents = New BV With {.child1 = sibling, .child2 = leafIndex}})
                newParent = New Tuple2 With {.X = bvList.Count - 1, .Y = -1}

                'set children's parent
                obj.parent = newParent.X
                objectList(leafIndex.X)(leafIndex.Y) = obj
                If bv Then 'have to change in the correct array for sibling
                    Dim s = bvList(sibling.X)
                    s.parent = newParent.X
                    bvList(sibling.X) = s
                Else
                    Dim s = objectList(sibling.X)(sibling.Y)
                    s.parent = newParent.X
                    objectList(sibling.X)(sibling.Y) = s
                End If

                'change the old parent so it references the new parent instead of sibling, while keeping its other child (if oldParent = -1, then change root instead)
                If oldParent <> -1 Then
                    If bvList(oldParent).contents.child1.X = sibling.X AndAlso bvList(oldParent).contents.child1.Y = sibling.Y Then 'child1 is sibling; change to new parent
                        Dim changed = bvList(oldParent)
                        changed.contents.child1 = newParent
                        bvList(oldParent) = changed

                    Else
                        Dim changed = bvList(oldParent)
                        changed.contents.child2 = newParent
                        bvList(oldParent) = changed

                    End If
                Else
                    root = newParent
                End If
            End If

            Return newParent
        End Function


        'input parent index (the parent to begin resizing from, inclusive)
        Public Sub Refitting(index As Integer)
            While index <> -1
                Dim toChange = bvList(index)

                Dim upper1 As Vector3
                Dim upper2 As Vector3
                Dim lower1 As Vector3
                Dim lower2 As Vector3

                'reference correct list & get bounds
                Dim c1 = bvList(index).contents.child1
                Dim c2 = bvList(index).contents.child2

                'if no children
                If c1.Y = -2 AndAlso c2.Y = -2 AndAlso index <> root.X Then
                    Throw New Exception("No children in BV")
                End If

                If c1.Y <> -2 Then 'valid child here
                    If c1.Y = -1 Then
                        Dim child = bvList(c1.X)
                        upper1 = child.upper
                        lower1 = child.lower
                    Else
                        Dim child = objectList(c1.X)(c1.Y)
                        upper1 = child.upper
                        lower1 = child.lower
                    End If
                End If

                If c2.Y <> -2 Then 'valid child here
                    If c2.Y = -1 Then
                        Dim child = bvList(c2.X)
                        upper2 = child.upper
                        lower2 = child.lower
                    Else
                        Dim child = objectList(c2.X)(c2.Y)
                        upper2 = child.upper
                        lower2 = child.lower
                    End If
                Else 'handle only one model
                    upper2 = upper1
                    lower2 = lower1
                End If

                If c1.Y = -2 Then 'handle only one model
                    upper1 = upper2
                    lower1 = lower2
                End If

                'update parent bounds
                toChange.upper = Max(upper1, upper2)
                toChange.lower = Min(lower1, lower2)
                bvList(index) = toChange

                Rotate(index) 'try rotations to improve balance of tree

                index = bvList(index).parent
            End While
        End Sub

        Sub Rotate(index As Integer)
            Dim current = bvList(index)
            Dim parent = bvList(index).parent
            If parent = -1 Then Exit Sub 'exit early if no parent (need a parent for the rotations)

            Dim max2, min2 As Vector3
            Dim other = bvList(parent).contents.child1
            Dim child1 As Boolean = True
            If other.Y = -1 AndAlso other.X = index Then 'must be this bv (need parent's other child)
                other = bvList(parent).contents.child2
                child1 = False
            End If

            'get max and min of other child
            If other.Y = -1 Then 'is a bv
                Dim c = bvList(other.X)
                max2 = c.upper
                min2 = c.lower
            Else
                Dim c = objectList(other.X)(other.Y)
                max2 = c.upper
                min2 = c.lower
            End If

            'calculate original surface area
            Dim origSA = SArea(current.upper, current.lower)

            Dim c1Max, c1Min As Vector3
            Dim c2Max, c2Min As Vector3
            If current.contents.child1.Y = -1 Then 'getting max and min of current children
                Dim c = bvList(current.contents.child1.X)
                c1Max = c.upper
                c1Min = c.lower
            Else
                Dim c = objectList(current.contents.child1.X)(current.contents.child1.Y)
                c1Max = c.upper
                c1Min = c.lower
            End If
            If current.contents.child2.Y = -1 Then
                Dim c = bvList(current.contents.child2.X)
                c2Max = c.upper
                c2Min = c.lower
            Else
                Dim c = objectList(current.contents.child2.X)(current.contents.child2.Y)
                c2Max = c.upper
                c2Min = c.lower
            End If

            Dim noC1 As Single 'if child1 was replaced
            Dim noC2 As Single 'if child2 was replaced

            'calculating new surface areas if rotations were to happen
            Dim testMax1 = Max(c1Max, max2)
            Dim testMin1 = Min(c1Min, min2)
            noC2 = SArea(testMax1, testMin1)

            Dim testMax2 = Max(c2Max, max2)
            Dim testMin2 = Min(c2Min, min2)
            noC1 = SArea(testMax2, testMin2)

            Dim best As Integer = 0 '1 to rotate child1, 2 to rotate child2
            If noC1 < noC2 AndAlso noC1 < origSA Then
                best = 1
            ElseIf noC2 < origSA Then
                best = 2
            End If

            If best <> 0 Then 'one of them must be better than current
                'perform rotation (set new size for this bv and change node references)
                Dim toRotate As Tuple2
                If best = 1 Then
                    current.upper = testMax2
                    current.lower = testMin2
                    toRotate = current.contents.child1
                    current.contents.child1 = other 'replace child
                ElseIf best = 2 Then
                    current.upper = testMax1
                    current.lower = testMin1
                    toRotate = current.contents.child2
                    current.contents.child2 = other
                End If

                Dim p = bvList(parent)
                If child1 Then 'finish rotating
                    p.contents.child1 = toRotate
                Else
                    p.contents.child2 = toRotate
                End If

                'set rotating children's parents (both other and toRotate)
                If other.Y = -1 Then
                    Dim c = bvList(other.X)
                    c.parent = index
                    bvList(other.X) = c
                Else
                    Dim c = objectList(other.X)(other.Y)
                    c.parent = index
                    objectList(other.X)(other.Y) = c
                End If
                If toRotate.Y = -1 Then
                    Dim c = bvList(toRotate.X)
                    c.parent = parent
                    bvList(toRotate.X) = c
                Else
                    Dim c = objectList(toRotate.X)(toRotate.Y)
                    c.parent = parent
                    objectList(toRotate.X)(toRotate.Y) = c
                End If

                bvList(parent) = p
                bvList(index) = current
            End If
        End Sub

        'used to find the best sibling for adding leaf to
        Private Function Pick(obj As AABB(Of Model)) As Tuple2
            'uses surface area heuristics to find the cost of a tree (as the total surface area of the internal nodes)
            'will be using the branch and bound algorithm to improve the search performance

            Dim priority As New PriorityQueue(Of Cost, Single) 'The single is used to order (by cost) 
            Dim best As New Cost With {.cost = Single.MaxValue, .contents = root, .prev = 0F}
            priority.Enqueue(New Cost With {.cost = 0F, .contents = root, .prev = 0F}, best.cost)

            'used for calculating the lower bound costs for child nodes
            Dim leafArea As Single = SArea(obj.upper, obj.lower)

            While priority.Count > 0
                Dim current = priority.Dequeue

                'continue if there is no valid child here
                If current.contents.Y = -2 Then Continue While

                'get bounds for current item
                Dim upper As Vector3
                Dim lower As Vector3
                If current.contents.Y = -1 Then 'is bv
                    Dim s = bvList(current.contents.X)
                    upper = s.upper
                    lower = s.lower
                Else
                    Dim s = objectList(current.contents.X)(current.contents.Y)
                    upper = s.upper
                    lower = s.lower
                End If
                Dim tUpper = Max(obj.upper, upper)
                Dim tLower = Min(obj.lower, lower)

                'get cost
                current.cost = SArea(tUpper, tLower) + current.prev
                If current.cost < best.cost Then
                    best = current
                End If

                'calculate lower bound cost (if has any children)
                If current.contents.Y = -1 Then 'is bv
                    Dim nodeCost = SArea(upper, lower) 'original node cost (without leaf)

                    Dim cSA = current.cost - nodeCost 'get change in cost for node (change in SA for current node)
                    Dim prev = cSA + current.prev '(change in SA for previous nodes)
                    Dim lowerBound As Single = leafArea + prev

                    'if potentially better, then add to priority queue
                    If lowerBound < best.cost Then
                        'create new items for the queue for each sibling
                        Dim child As New Cost With {
                        .prev = prev, .cost = lowerBound}

                        Dim s = bvList(current.contents.X)
                        child.contents = s.contents.child1
                        priority.Enqueue(child, child.cost)

                        child.contents = s.contents.child2
                        priority.Enqueue(child, child.cost)
                    End If
                End If
            End While

            Return best.contents 'return the index of the element with lowest cost
        End Function


        'if an intersection is found, then it will return the index of the object (atm, only checks aabbs and bvs - not the bsp trees)
        'if not, will return Y = -2
        Public Sub CheckIntersect(r As Ray3, ByRef selected As Model, ByRef ui3dSelected As Model, lightSpheresHidden As Boolean)
            Dim pQ As New PriorityQueue(Of Tuple(Of Model, Single), Single) 'a single that represents tMin (the shortest point from ray to aabb chosen in cost)
            TestIntersect(r, root, pQ, lightSpheresHidden)
            If pQ.Count = 0 Then
                ui3dSelected = Nothing 'ensure that any 3d ui will be unselected
                selected = Nothing
            Else
                Dim s = pQ.Dequeue 'returns the closest valid aabb
                If s.Item2 <= 0 AndAlso selected IsNot Nothing Then 'is a depth0 object (3d ui), and another model is currently selected
                    'get all of the 3dui items and select the last one (the one that is closest to 0)
                    Dim prev = s
                    While s.Item2 <= 0 AndAlso pQ.Count > 0
                        s = pQ.Dequeue
                        If s.Item2 <= 0 Then
                            prev = s
                        End If
                    End While

                    ui3dSelected = prev.Item1
                Else
                    If s.Item2 > 0 Then
                        ui3dSelected = Nothing
                        selected = s.Item1
                    Else
                        Dim prev = s
                        While s.Item2 <= 0 AndAlso pQ.Count > 0
                            s = pQ.Dequeue
                        End While
                        If s.Item2 > 0 Then
                            selected = s.Item1
                        Else
                            selected = Nothing
                        End If
                    End If
                End If
            End If
        End Sub

        Private Sub TestIntersect(r As Ray3, current As Tuple2, pQ As PriorityQueue(Of Tuple(Of Model, Single), Single), lightSpheresHidden As Boolean)
            If current.X = -1 Then Exit Sub

            Dim rDir = New Vector3(1 / r.dir.X, 1 / r.dir.Y, 1 / r.dir.Z)
            Dim min As Vector3
            Dim max As Vector3

            'get bounds
            Dim isDepth0 As Boolean = False
            If current.Y = -1 Then
                Dim s = bvList(current.X)
                min = s.lower
                max = s.upper
            Else
                Dim s = objectList(current.X)(current.Y)
                min = s.lower
                max = s.upper
                If s.contents.depth0 Then
                    isDepth0 = True
                End If
            End If

            Dim tLower = (min - r.origin) * rDir
            Dim tUpper = (max - r.origin) * rDir

            'perform bv intersection tests
            Dim tMax = tLower.Z.Max(tUpper.Z).Min(tLower.Y.Max(tUpper.Y).Min(tLower.X.Max(tUpper.X))) 'get the max of each x/y/z, and then find the overall min
            If tMax < 0 Then Exit Sub

            Dim tMin = tLower.Z.Min(tUpper.Z).Max(tLower.Y.Min(tUpper.Y).Max(tLower.X.Min(tUpper.X))) 'get the min of each x/y/z, and then find the overall max
            If tMin > tMax Then Exit Sub

            If current.Y = -1 Then 'check intersections with child bvs
                TestIntersect(r, bvList(current.X).contents.child1, pQ, lightSpheresHidden)
                TestIntersect(r, bvList(current.X).contents.child2, pQ, lightSpheresHidden)

            Else 'model aabb intersection (add to priority queue)
                'will do bsp tree and triangle intersection tests here
                Dim hasIntersect As Boolean = False

                'create a new ray that will be transformed back to default plane (like in InputQuery) - probably need to translate first (don't need to translate back since bsp tree treats object as if it has no transformations (default as loaded in)
                Dim obj = objectList(current.X)(current.Y)
                If obj.contents.lightRef <> -1 AndAlso lightSpheresHidden Then Exit Sub 'skip this object if it is a light sphere that is hidden (ie. shouldn't be interactable)

                Dim iRay = r
                iRay.origin += obj.contents.translation

                Dim oRot = obj.contents.rot
                Dim matrix As Pipeline.Transform
                Pipeline.TransformMatrix(obj.contents.scale, obj.contents.rot, New Vector3, matrix)
                MatrixOp.Transpose(matrix, matrix)

                'get inverse matrix
                MatrixOp.Inverse(matrix, matrix)

                Dim origin As New Vector4(iRay.origin, 1)
                Dim dir As New Vector4(iRay.dir, 1)

                iRay.origin += obj.contents.translation
                Dim origin2 As New Vector4(iRay.origin, 1)
                Dim dir2 As New Vector4(iRay.dir, 1)

                MatrixOp.Multiply(origin, matrix, origin)
                MatrixOp.Multiply(dir, matrix, dir)

                iRay.origin = New Vector3(origin.X, origin.Y, origin.Z)
                iRay.dir = New Vector3(dir.X, dir.Y, dir.Z)
                iRay.dir = Vector3.Normalize(iRay.dir)

                'then call the bsp intersection code
                Dim currentBSP = obj.contents.contents.oBsp
                If Not IsNothing(currentBSP) Then
                    hasIntersect = currentBSP.CheckIntersect(iRay)
                End If

                If Not hasIntersect Then 'check the transparent bsp tree
                    currentBSP = obj.contents.contents.tBsp
                    If Not IsNothing(currentBSP) Then
                        hasIntersect = currentBSP.CheckIntersect(iRay)
                    End If
                End If

                If hasIntersect Then
                    If isDepth0 Then 'ensure 3d ui are added to front of queue, and that regular ui only has positive tMin values
                        tMin = -Math.Abs(tMin)
                    Else
                        tMin = Math.Abs(tMin)
                    End If
                    pQ.Enqueue(New Tuple(Of Model, Single)(obj.contents, tMin), tMin)
                End If
            End If
        End Sub

        Public Sub Iterate(sc As Scene, ByRef models() As Model, cam As Camera, opt As Pipeline.RenderOptions)
            Dim total As Integer
            For i = 0 To objectList.Count - 1
                If objectList(i) Is Nothing Then Continue For
                For m = 0 To objectList(i).Count - 1
                    If objectList(i)(m).contents Is Nothing Then Continue For
                    total += 1
                Next
            Next

            Dim viewDir = sc.GetMouseRay(New Point(cam.resX / 2, cam.resY / 2), cam)
            Dim tr As New List(Of Model)
            Dim op As New List(Of Model)

            'max angle possible should be from centre to the very corner of the screen (given a cone shape)
            Dim corner = sc.GetMouseRay(New Point(cam.resX, cam.resY), cam)
            Dim cDir = Vector3.Normalize(corner.dir)

            Dim dir = Vector3.Normalize(viewDir.dir)
            Dim range As Single = Math.Acos(Vector3.Dot(dir, cDir))

            Iteration(root, tr, op, cam, opt, dir, range)

            'sort the transparent list
            If tr.Count > 0 Then
                Quicksort(tr, 0, tr.Count)
            End If

            'combine them both into the same array (may not need two passes dependant on how the models are combined)
            ReDim models(tr.Count + sc.arrows.Length + op.Count - 1)
            Dim current = 0
            For Each ui In sc.arrows
                models(current) = ui
                current += 1
            Next
            If tr.Count > 0 Then
                For i = 0 To tr.Count - 1
                    models(current) = tr(i)
                    current += 1
                Next
            End If
            If op.Count > 0 Then
                For i = 0 To op.Count - 1
                    models(current) = op(i)
                    current += 1
                Next
            End If
            If opt.toVBuffer Then Debug.Print(models.Length) 'debugging
        End Sub

        'should only call this with current being a bv
        Private Sub Iteration(current As Tuple2, tr As List(Of Model), op As List(Of Model), cam As Camera, opt As Pipeline.RenderOptions, viewDir As Vector3, range As Single)
            Dim ch1 = bvList(current.X).contents.child1
            Dim ch2 = bvList(current.X).contents.child2

            Dim min1 As Vector3
            Dim max1 As Vector3
            Dim min2 As Vector3
            Dim max2 As Vector3
            If ch1.Y = -1 Then 'bv
                Dim c1 = bvList(ch1.X)
                max1 = c1.upper
                min1 = c1.lower
            Else 'aabb
                Dim c1 = objectList(ch1.X)(ch1.Y)
                max1 = c1.upper
                min1 = c1.lower

            End If
            If ch2.Y = -1 Then 'bv
                Dim c2 = bvList(ch2.X)
                max2 = c2.upper
                min2 = c2.lower
            Else 'aabb
                Dim c2 = objectList(ch2.X)(ch2.Y)
                max2 = c2.upper
                min2 = c2.lower
            End If

            Dim closest1 = Vector3.Clamp(cam.pos, min1, max1)
            Dim closest2 = Vector3.Clamp(cam.pos, min2, max2)

            Dim angle1 As Single = Single.MaxValue
            Dim angle2 As Single = Single.MaxValue

            'first bv
            Dim points() As Vector3 = {
                    min1,
                    New Vector3(min1.X, min1.Y, max1.Z),
                    New Vector3(min1.X, max1.Y, min1.Z),
                    New Vector3(min1.X, max1.Y, max1.Z),
                    New Vector3(max1.X, min1.Y, min1.Z),
                    New Vector3(max1.X, min1.Y, max1.Z),
                    New Vector3(max1.X, max1.Y, min1.Z),
                    max1
                }
            For i = 0 To points.Length - 1
                points(i) = Vector3.Normalize(points(i) - cam.pos)
            Next

            'second bv
            Dim points2() As Vector3 = {
                    min2,
                    New Vector3(min2.X, min2.Y, max2.Z),
                    New Vector3(min2.X, max2.Y, min2.Z),
                    New Vector3(min2.X, max2.Y, max2.Z),
                    New Vector3(max2.X, min2.Y, min2.Z),
                    New Vector3(max2.X, min2.Y, max2.Z),
                    New Vector3(max2.X, max2.Y, min2.Z),
                    max2
                }
            For i = 0 To points2.Length - 1
                points2(i) = Vector3.Normalize(points2(i) - cam.pos)
            Next

            'check lines (calculate nearest point to viewDir on line)
            For i = 0 To points.Length - 1
                For n = 0 To points.Length - 1
                    If n = i Then Continue For
                    Dim lineDir = Vector3.Normalize(points(i) - points(n))
                    Dim v = viewDir - points(n)
                    Dim d = Vector3.Dot(v, lineDir)
                    d = Single.Clamp(d, 0, (points(i) - points(n)).Length) 'clamp between the two points on the line
                    Dim final = Vector3.Normalize(points(n) + lineDir * d)
                    Dim angle = Math.Acos(Vector3.Dot(viewDir, final)) 'calculating angle
                    If Single.IsNaN(angle) OrElse angle < angle1 Then angle1 = angle
                Next
                If angle1 < range Then Exit For
            Next
            For i = 0 To points2.Length - 1
                For n = 0 To points2.Length - 1
                    If n = i Then Continue For
                    Dim lineDir = Vector3.Normalize(points2(i) - points2(n))
                    Dim v = viewDir - points2(n)
                    Dim d = Vector3.Dot(v, lineDir)
                    d = Single.Clamp(d, 0, (points2(i) - points2(n)).Length)
                    Dim final = Vector3.Normalize(points2(n) + lineDir * d)
                    Dim angle = Math.Acos(Vector3.Dot(viewDir, final))
                    If Single.IsNaN(angle) OrElse angle < angle2 Then angle2 = angle
                Next
                If angle2 < range Then Exit For
            Next

            'if valid, travel down the valid branches, prioritising the branch with lower closest value (since the resultant list is more likely to be sorted)
            'if they are a model, can save closest for the sorting later
            Dim depth1 As Single = -1 'depth should always be to the closest point, but the angle calculations need to be on the box (more precise)
            Dim depth2 As Single = -1
            If Single.IsNaN(angle1) OrElse angle1 < range Then
                depth1 = (closest1 - cam.pos).Length
            End If
            If Single.IsNaN(angle2) OrElse angle2 < range Then
                depth2 = (closest2 - cam.pos).Length
            End If

            If depth1 <= depth2 Then
                If depth1 <> -1 Then
                    If ch1.Y = -1 Then
                        Iteration(ch1, tr, op, cam, opt, viewDir, range)
                    Else 'is a model; always skip the 3d ui (that can be added last)
                        Dim obj = objectList(ch1.X)(ch1.Y).contents
                        obj.closest = depth1
                        If (opt.transparency = 1 AndAlso obj.contents.hasTransparent AndAlso Not obj.depth0) Then
                            tr.Add(obj)
                        ElseIf Not obj.depth0 Then
                            op.Add(obj)
                        End If
                    End If
                End If
                If depth2 <> -1 Then
                    If ch2.Y = -1 Then
                        Iteration(ch2, tr, op, cam, opt, viewDir, range)
                    Else 'is a model
                        Dim obj = objectList(ch2.X)(ch2.Y).contents
                        obj.closest = depth2
                        If (opt.transparency = 1 AndAlso obj.contents.hasTransparent AndAlso Not obj.depth0) Then
                            tr.Add(obj)
                        ElseIf Not obj.depth0 Then
                            op.Add(obj)
                        End If
                    End If
                End If
            Else
                If depth2 <> -1 Then
                    If ch2.Y = -1 Then
                        Iteration(ch2, tr, op, cam, opt, viewDir, range)
                    Else 'is a model
                        Dim obj = objectList(ch2.X)(ch2.Y).contents
                        obj.closest = depth2
                        If (opt.transparency = 1 AndAlso obj.contents.hasTransparent AndAlso Not obj.depth0) Then
                            tr.Add(obj)
                        ElseIf Not obj.depth0 Then
                            op.Add(obj)
                        End If
                    End If
                End If
                If depth1 <> -1 Then
                    If ch1.Y = -1 Then
                        Iteration(ch1, tr, op, cam, opt, viewDir, range)
                    Else 'is a model
                        Dim obj = objectList(ch1.X)(ch1.Y).contents
                        obj.closest = depth1
                        If (opt.transparency = 1 AndAlso obj.contents.hasTransparent AndAlso Not obj.depth0) Then
                            tr.Add(obj)
                        ElseIf Not obj.depth0 Then
                            op.Add(obj)
                        End If
                    End If
                End If
            End If
        End Sub

        Sub Quicksort(ByRef sort As List(Of Model), ByVal left As Integer, ByVal right As Integer)
            Dim length As Integer = right - left

            Dim pivot As Integer
            If length > 1 Then
                pivot = (left + right) / 2

                'partition (ie. sort the pivot so it is ordered)
                Dim i As Integer, piv As Model, store As Integer

                piv = sort(pivot)

                'put pivot at the end
                Dim tmp As Model
                tmp = sort(right - 1)
                sort(right - 1) = sort(pivot)
                sort(pivot) = tmp
                store = left

                Dim point1 As Single = piv.closest

                For i = left To right - 2

                    Dim point2 As Single = sort(i).closest

                    'for depth0
                    If piv.depth0 AndAlso sort(i).depth0 Then
                        Continue For
                    End If

                    If point2 <= point1 Then 'compare distance to camera
                        'switch here (sorting the pivot)
                        tmp = sort(store)
                        sort(store) = sort(i)
                        sort(i) = tmp

                        store += 1
                    End If
                Next

                'switch so that pivot is where store is (ie. next to where the last swapped item is)
                tmp = sort(store)
                sort(store) = sort(right - 1)
                sort(right - 1) = tmp

                'set pivot
                pivot = store

                'recursively sort sub-arrays (all the values to the left will be under current pivot, all values to right will be over current pivot // depending on direction given)
                Quicksort(sort, left, pivot)
                Quicksort(sort, pivot + 1, right)
            End If
        End Sub

        'expects max as first and min as second
        Public Function SArea(max As Vector3, min As Vector3) As Single
            Dim size = max - min
            Return 2.0F * ((size.X * size.Z) + (size.Z * size.Y) + (size.X * size.Y)) 'using 2(xz + zy + xy)
        End Function

        Public Function Max(v1 As Vector3, v2 As Vector3) As Vector3
            Dim m As Vector3

            If v1.X > v2.X Then m.X = v1.X Else m.X = v2.X
            If v1.Y > v2.Y Then m.Y = v1.Y Else m.Y = v2.Y
            If v1.Z > v2.Z Then m.Z = v1.Z Else m.Z = v2.Z

            Return m
        End Function
        Public Function Min(v1 As Vector3, v2 As Vector3) As Vector3
            Dim m As Vector3

            If v1.X < v2.X Then m.X = v1.X Else m.X = v2.X
            If v1.Y < v2.Y Then m.Y = v1.Y Else m.Y = v2.Y
            If v1.Z < v2.Z Then m.Z = v1.Z Else m.Z = v2.Z

            Return m
        End Function
    End Class
End Class

Public Class Pipeline
    Public Structure RenderOptions
        Public asWireFrame As Boolean

        Public toZBuffer As Boolean
        Public toVBuffer As Boolean

        Public lighting As Boolean
        Public shadowRes As Integer

        Public isSkybox As Boolean

        Public cullBack As Integer '1 => cull back; -1 => cull front; 0 => cull neither
        Public hideLightSpheres As Boolean 'need to add this functionality (if this is true, then can just skip models that have light references)
        Public transparency As Integer 'determines if transparent polygons should be ignored(-1), drawn opaque(0) or blended(1) (separate bsp tree used for transparent polygons)
        'when in the rasterizer, 1 leads to only transparent polygons being drawn (with blending checks), 0 leads to both opaque and transparent ones being drawn (without alpha), and -1 only draws opaque polygons
    End Structure

    Public Structure VPoints
        Dim v1 As Vector4
        Dim v2 As Vector4
        Dim v3 As Vector4

        Default Property Val(index As Integer) As Vector4
            Get
                If index = 0 Then Return v1
                If index = 1 Then Return v2
                If index = 2 Then Return v3
            End Get
            Set(value As Vector4)
                If index = 0 Then
                    v1 = value
                ElseIf index = 1 Then
                    v2 = value
                ElseIf index = 2 Then
                    v3 = value
                End If
            End Set
        End Property
    End Structure
    Public Structure TPoints
        Dim t1 As Vector3
        Dim t2 As Vector3
        Dim t3 As Vector3

        Default Property Val(index As Integer) As Vector3
            Get
                If index = 0 Then Return t1
                If index = 1 Then Return t2
                If index = 2 Then Return t3
            End Get
            Set(value As Vector3)
                If index = 0 Then
                    t1 = value
                ElseIf index = 1 Then
                    t2 = value
                ElseIf index = 2 Then
                    t3 = value
                End If
            End Set
        End Property

    End Structure

    Public Structure Transform
        Dim x As Vector4
        Dim y As Vector4
        Dim z As Vector4
        Dim w As Vector4
    End Structure

    Private Structure Options
        Dim isTop As Boolean
        Dim isParallel As Boolean
    End Structure

    'the following structures are used for calculations within the triangle rasterizing subroutines
    Private Structure Sides
        Dim cY As Integer
        Dim xB As Single
        Dim currentX As Single
        Dim invSlope As Single
    End Structure

    Private Structure Bary
        Dim part3 As Single
        Dim part4 As Single
        Dim rDeterminant As Single
    End Structure

    Private Structure Interpolation
        Dim b As Bary

        Dim v1 As Vector4 'used for storing ndc coords
        Dim v2 As Vector4
        Dim v3 As Vector4

        Dim l1 As Vector4
        Dim l2 As Vector4
        Dim l3 As Vector4
    End Structure

    Private Structure Tri
        Dim v1 As Vector4
        Dim v2 As Vector4
        Dim v3 As Vector4

        Dim multTex1 As Vector2
        Dim multTex2 As Vector2
        Dim multTex3 As Vector2

        Dim rZ1 As Single
        Dim rZ2 As Single
        Dim rZ3 As Single

        Dim opt As Options
        Dim side1 As Sides
        Dim side2 As Sides

        Dim toInterpolate As Interpolation
        Dim normal As Vector3 'used for lighting (intensity)
    End Structure

    Dim temp() As Vector4 'storage for transformed vertices
    Dim obj As Model
    Dim cam As Camera
    Dim lights() As PointLight

    Dim rOptions() As RenderOptions 'using an array leads to faster access times for threads 

    'thread stuff
    Dim threadVBuffers() As List(Of VPoints) 'local storage for threads
    Dim threadTBuffers() As List(Of TPoints)
    Dim threadLocks() As ManualResetEventSlim 'to manage threads
    Dim stopThreads As Boolean 'will stop all render threads from running   
    Dim current() As Integer
    Dim stopPos() As Integer
    Dim size As Integer = 300
    Dim main() As ManualResetEventSlim

    Dim pTr As Transform
    Dim inv As Transform
    Dim depth0 As Boolean

    Dim xBound As Single
    Dim tr As Transform
    Dim cull As Integer
    Dim objInv As Transform

    'amount of threads used for rendering
    Public Sub New(threadCount As Integer)
        ReDim threadVBuffers(threadCount - 1)
        ReDim threadTBuffers(threadCount - 1)
        ReDim threadLocks(threadCount - 1)
        ReDim current(threadCount - 1)
        ReDim stopPos(threadCount - 1)
        ReDim main(threadCount - 1)
        ReDim rOptions(threadCount - 1)

        For i = 0 To threadCount - 1
            threadVBuffers(i) = New List(Of VPoints)
            threadTBuffers(i) = New List(Of TPoints)

            threadLocks(i) = New ManualResetEventSlim
            threadLocks(i).Reset()
            main(i) = New ManualResetEventSlim
            main(i).Set() 'start unlocked

            Dim current = i
            Dim newThread As New Thread(Sub() RenderTriangles(current))
            newThread.Start() 'render threads will now wait for triangles to be given to them
        Next
    End Sub


    Public Sub RenderPass(scene As Scene, cam As Camera, lights() As PointLight, skybox As Model, opt As RenderOptions, bmp As WriteableBitmap, bPtr As IntPtr, screen As Control)
        If cam.vBuffer Is Nothing OrElse cam.vBuffer.Length <> cam.resX * cam.resY Then InitialiseBuffers(cam, cam.resX, cam.resY, opt) 'rescales buffers if needed
        Me.lights = lights

        Dim light As RenderOptions
        If opt.lighting AndAlso lights.Length > 0 Then 'draw to light maps

            light = New RenderOptions With {.lighting = False, .toVBuffer = False, .toZBuffer = True, .transparency = opt.transparency, .cullBack = -1, .hideLightSpheres = True}
            If opt.transparency = 1 Then 'lights shouldn't render transparent polygons if transparency is enabled or ignored (to avoid transparent objects completely blocking light)
                light.transparency = -1
            End If

            For i = 0 To lights.Length - 1
                If lights(i).Range = 0 Then 'deleted light
                    Continue For
                End If
                For l = 0 To 5
                    If lights(i).cams(l) Is Nothing Then lights(i).cams(l) = New Camera
                    If lights(i).cams(l).zBuffer Is Nothing OrElse lights(i).cams(l).zBuffer.Length <> opt.shadowRes * opt.shadowRes Then InitialiseBuffers(lights(i).cams(l), opt.shadowRes, opt.shadowRes, light)

                    Dim dir As Vector3 'get correct direction
                    If l = 0 Then
                        dir = New Vector3 'front facing (towards -ve z axis)
                        lights(i).cams(l).currentWDir = New Vector3(0, 0, -1)
                    ElseIf l = 1 Then
                        dir = New Vector3(0, Math.PI, 0) 'behind facing
                        lights(i).cams(l).currentWDir = New Vector3(0, 0, 1)
                    ElseIf l = 2 Then
                        dir = New Vector3(0, -Math.PI / 2, 0) 'left facing (towards -ve x?)
                        lights(i).cams(l).currentWDir = New Vector3(-1, 0, 0)
                    ElseIf l = 3 Then
                        dir = New Vector3(0, Math.PI / 2, 0) 'right facing
                        lights(i).cams(l).currentWDir = New Vector3(1, 0, 0)
                    ElseIf l = 4 Then
                        dir = New Vector3(Math.PI / 2, 0, 0) 'up facing (towards +ve y)
                        lights(i).cams(l).currentWDir = New Vector3(0, 1, 0)
                    ElseIf l = 5 Then
                        dir = New Vector3(-Math.PI / 2, 0, 0) 'down facing
                        lights(i).cams(l).currentWDir = New Vector3(0, -1, 0)
                    End If

                    'set cams details
                    lights(i).cams(l).fov = 90 '90 for full view (90/2=45 degrees each side; covers cube; remember that 180 fov doesn't work because 90 each side = infinite frustum)
                    lights(i).cams(l).near = 0.1 'smaller near and closer far because light is expected to only cover closer objects
                    lights(i).cams(l).far = 1000
                    lights(i).cams(l).pos = lights(i).pos
                    lights(i).cams(l).dir = dir
                    lights(i).cams(l).resX = opt.shadowRes
                    lights(i).cams(l).resY = opt.shadowRes

                    DrawToBuffer(scene, lights(i).cams(l), light)
                Next
            Next
        End If

        DrawToBuffer(scene, cam, opt)

        Display(bmp, bPtr, cam, screen)
        ClearBuffers(cam, opt)

        'clear the shadowmap buffers
        If opt.lighting AndAlso lights.Length > 0 Then
            For i = 0 To lights.Length - 1
                If lights(i).Range = 0 Then Continue For 'deleted light
                For l = 0 To 5
                    ClearBuffers(lights(i).cams(l), light)
                Next
            Next
        End If

        Me.lights = Nothing
    End Sub

    Private Sub DrawToBuffer(scene As Scene, cam As Camera, opt As RenderOptions)
        Dim tr As Transform
        cam.zTransform = ProjMatrix(cam, pTr)
        cam.tr = pTr

        Dim objects(-1) As Model
        scene.sc.Iterate(scene, objects, cam, opt)

        Dim start = objects.Length - 1
        Dim endP = 0
        Dim orig = opt.transparency

        For i = start To endP Step -1 'always -1 for transparency (when rendering 3d ui)
            obj = objects(i)
            If obj IsNot Nothing AndAlso Not (obj.lightRef <> -1 AndAlso opt.hideLightSpheres) AndAlso obj.scale <> New Vector3 AndAlso Not (Not opt.toVBuffer AndAlso obj.depth0) Then 'ensure a valid object has been passed in; also skip if it is a light sphere and light spheres are disabled; also skip 3d ui if it is a light
                Me.cam = cam

                If obj.depth0 Then 'will always render this transparent
                    depth0 = True
                    opt.transparency = 1
                ElseIf Not obj.contents.hasTransparent Then
                    depth0 = False
                    opt.transparency = -1
                Else
                    depth0 = False
                    opt.transparency = orig
                End If

                For l = 0 To rOptions.Length - 1 'set render options for each thread
                    rOptions(l) = opt
                Next

                Dim transform As Pipeline.Transform
                TransformMatrix(objects(i).scale, objects(i).rot, objects(i).translation, transform)
                MatrixOp.Multiply(pTr, transform, tr)

                'inverse matrix (and transformation per object)
                If opt.lighting Then
                    MatrixOp.Inverse(tr, inv)
                    MatrixOp.Transpose(inv, inv)
                    MatrixOp.Transpose(transform, transform)
                    MatrixOp.Multiply(transform, inv, inv)
                End If

                'calculate depth to centre
                Dim v As Vector4
                MatrixOp.Multiply(New Vector4(New Vector3, 1), tr, v)
                obj.depthToCentre = v.W

                'get obj inverse
                Dim objTr As Transform
                TransformMatrix(obj.scale, obj.rot, obj.translation, objTr)
                MatrixOp.Transpose(objTr, objTr)
                MatrixOp.Inverse(objTr, objInv)

                DrawTriangles(tr)
            End If
        Next

        obj = Nothing 'ensure there are no leftover handles in case the objects are deleted later
        Me.cam = Nothing
    End Sub

    <MethodImpl(MethodImplOptions.AggressiveOptimization)>
    Private Sub DrawTriangles(tr As Transform)
        TransformTriangles(tr)
        Me.tr = tr

        Dim triCount As Integer = obj.contents.index.Length \ 3
        Dim tCount = threadLocks.Length

        cull = rOptions(0).cullBack

        xBound = 2 / tCount

        Dim oBSP As Base.BSPTree = Nothing
        Dim tBSP As Base.BSPTree = Nothing
        Dim dir2 As Integer

        If rOptions(0).transparency = 1 Then 'only render transparent polygons (also disable cull)
            tBSP = obj.contents.tBsp
            If obj.contents.hasTransparent Then
                oBSP = obj.contents.oBsp
            End If
            dir2 = 1
            cull = 0 '-1
            If depth0 Then 'makes the 3d ui look nicer to the user
                cull = -1
            End If
        ElseIf rOptions(0).transparency = -1 Then 'only render opaque polygons
            oBSP = obj.contents.oBsp
        Else 'render both as opaque (could have two variables set to nothing, can traverse any that have been set - either by -1, 1, or 0 (both) transparency settings
            oBSP = obj.contents.oBsp
            tBSP = obj.contents.tBsp
            dir2 = -1
        End If

        If IsNothing(oBSP) AndAlso IsNothing(tBSP) Then 'either: doesn't have any triangles; doesn't have any transparent triangles (if tr = 1); doesn't have any opaque triangles (if tr = -1)
            Exit Sub
        Else
            If Not IsNothing(oBSP) Then
                For i = 0 To oBSP.sort.Length - 1
                    ManageTriangles(oBSP.sort(i))
                Next
            End If
            If Not IsNothing(tBSP) Then
                If rOptions(0).transparency = 1 Then

                    'before calling iterate, will need to transform camera by the inverse matrix of object 
                    Dim newPos As New Vector4(cam.pos, 1)
                    MatrixOp.Multiply(newPos, objInv, newPos)
                    Dim newCam = New Camera With {.dir = cam.dir, .fov = cam.fov, .pos = New Vector3(newPos.X, newPos.Y, newPos.Z), .resX = cam.resX, .resY = cam.resY}

                    Dim result = tBSP.Iterate(dir2, newCam)
                    For Each node In result
                        For Each tri In node.self.triangles
                            ManageTriangles(tri)
                        Next
                    Next
                Else
                    For i = 0 To tBSP.sort.Length - 1
                        ManageTriangles(tBSP.sort(i))
                    Next
                End If
            End If
        End If

        'ensure all threads will finish work
        For i = 0 To tCount - 1
            While threadLocks(i).IsSet : main(i).Wait() : End While 'wait for current iteration to finish (avoid race conditions)
            If stopPos(i) < threadTBuffers(i).Count - 1 Then
                stopPos(i) = threadTBuffers(i).Count - 1
            End If
            If stopPos(i) = 0 AndAlso threadTBuffers(i).Count = 1 Then current(i) = -1 'special case

            threadLocks(i).Set() 'ensure render threads will render everything
        Next

        'wait for threads to finish work
        For i = 0 To tCount - 1
            While threadLocks(i).IsSet : main(i).Wait() : End While
            threadVBuffers(i).Clear()
            threadTBuffers(i).Clear()

            current(i) = 0
            stopPos(i) = 0
        Next
    End Sub

    <MethodImpl(MethodImplOptions.AggressiveOptimization)>
    Sub ManageTriangles(i As Integer)
        Dim xB = xBound
        Dim triIndex As Integer = i
        Dim tCount = threadLocks.Length

        Dim i1I As Base.Tuple3 = obj.contents.index(triIndex)
        Dim i2I As Base.Tuple3 = obj.contents.index(triIndex + 1)
        Dim i3I As Base.Tuple3 = obj.contents.index(triIndex + 2)

        Dim vPoints As New VPoints With {
            .v1 = temp(i1I.X - 1),
            .v2 = temp(i2I.X - 1),
            .v3 = temp(i3I.X - 1)}

        'culling triangles
        If (vPoints.v1.X < -1 AndAlso vPoints.v2.X < -1 AndAlso vPoints.v3.X < -1) OrElse (vPoints.v1.X > 1 AndAlso vPoints.v2.X > 1 AndAlso vPoints.v3.X > 1) Then 'too far left or right
            Exit Sub
        End If
        If (vPoints.v1.Y < -1 AndAlso vPoints.v2.Y < -1 AndAlso vPoints.v3.Y < -1) OrElse (vPoints.v1.Y > 1 AndAlso vPoints.v2.Y > 1 AndAlso vPoints.v3.Y > 1) Then 'too far up or down
            Exit Sub
        End If
        If vPoints.v1.W <= cam.near AndAlso vPoints.v2.W <= cam.near AndAlso vPoints.v3.W <= cam.near Then
            Exit Sub
        End If

        Dim tPoints As TPoints
        Dim currentTexture = i1I.Z

        If vPoints.v1.W <= cam.near OrElse vPoints.v2.W <= cam.near OrElse vPoints.v3.W <= cam.near Then 'fully behind or on near plane
            tPoints = New TPoints With {
            .t1 = New Vector3(obj.contents.texCoord(i1I.Y - 1), currentTexture),
            .t2 = New Vector3(obj.contents.texCoord(i2I.Y - 1), currentTexture),
            .t3 = New Vector3(obj.contents.texCoord(i3I.Y - 1), currentTexture)}

            Dim wPoints As New VPoints With {
                .v1 = New Vector4(obj.contents.vertex(i1I.X - 1), 1),
                .v2 = New Vector4(obj.contents.vertex(i2I.X - 1), 1),
                .v3 = New Vector4(obj.contents.vertex(i3I.X - 1), 1)}

            'xBoundary = xB
            ClipTriangles(wPoints, vPoints, tPoints, tr, cam.near, 3, 1, 1) 'clip triangles to the near plane
            Exit Sub
        End If

        Dim ab2 As Vector4 = vPoints.v2 - vPoints.v1 'backface cull
        Dim ac2 As Vector4 = vPoints.v3 - vPoints.v1
        Dim sign As Single = (ab2.X * ac2.Y) - (ac2.X * ab2.Y)
        If sign * cull < 0 Then
            Exit Sub
        End If

        tPoints = New TPoints With {
            .t1 = New Vector3(obj.contents.texCoord(i1I.Y - 1), currentTexture),
            .t2 = New Vector3(obj.contents.texCoord(i2I.Y - 1), currentTexture),
            .t3 = New Vector3(obj.contents.texCoord(i3I.Y - 1), currentTexture)}

        'distribute triangles
        'separating into strips by line (separate by y axis)
        'where vPoints contains each vertex of the current triangle (v1, v2, v3)
        For t = 0 To tCount - 1
            'get bounds
            Dim upper = xB * t
            Dim lower = xB * (t + 1)
            If t = tCount - 1 Then lower = 2 'ensure the entire screen is filled (regardless of precision)

            If (vPoints.v1.X <= 1 - upper OrElse vPoints.v2.X <= 1 - upper OrElse vPoints.v3.X <= 1 - upper) AndAlso (vPoints.v1.X >= 1 - lower OrElse vPoints.v2.X >= 1 - lower OrElse vPoints.v3.X >= 1 - lower) Then 'within this portion
                threadVBuffers(t).Add(vPoints)
                threadTBuffers(t).Add(tPoints)
                If ((threadTBuffers(t).Count - 1) - stopPos(t) > size) OrElse stopPos(t) = 0 Then 'wake up render thread; start immediately at beginning to hide the cost of this code
                    stopPos(t) = threadTBuffers(t).Count - 1
                    threadLocks(t).Set()
                End If
            End If
        Next
    End Sub

    'clip triangles past the near plane
    Private Sub ClipTriangles(world As VPoints, proj As VPoints, tex As TPoints, tr As Transform, boundary As Single, axis As Integer, stage As Integer, facing As Integer)
        'proj coords will be used for clipping, and world coords will be used to build ray 
        'world coords needed because proj coords are not linear and cannot get new proj coords without converting from world coords
        Dim addTri As Boolean
        Dim secondW As VPoints
        Dim secondP As VPoints
        Dim secondTex As TPoints

        Dim xBoundary = 2 / threadLocks.Length

        'only start clipping if points cross the boundary
        Dim start As Boolean
        If facing = 1 Then
            If proj.v1.Index(axis) <= boundary OrElse proj.v2.Index(axis) <= boundary OrElse proj.v3.Index(axis) <= boundary Then start = True
        Else
            If proj.v1.Index(axis) >= boundary OrElse proj.v2.Index(axis) >= boundary OrElse proj.v3.Index(axis) >= boundary Then start = True
        End If

        If start Then
            Dim side1 As Boolean 'used to check for valid sides (both original and intersections)
            Dim side2 As Boolean
            Dim side3 As Boolean
            Dim collision As Boolean 'used to check if there are any valid intersections

            'set for rays to work properly (need to set this equal to whatever proj is for the axis)
            world.v1.W = proj.v1.Index(axis)
            world.v2.W = proj.v2.Index(axis)
            world.v3.W = proj.v3.Index(axis)

            Dim ray1 As New Ray4 With {.origin = world.v1, .dir = world.v2 - world.v1, .min = 0, .max = 1}
            Dim ray2 As New Ray4 With {.origin = world.v2, .dir = world.v3 - world.v2, .min = 0, .max = 1}
            Dim ray3 As New Ray4 With {.origin = world.v3, .dir = world.v1 - world.v3, .min = 0, .max = 1}
            Dim t As Single

            'first side
            Dim p1 As Single = (boundary - proj.v1.Index(axis)) * -facing 'where +ve indicates it is on the side that the plane is facing (so a valid point)
            Dim p2 As Single = (boundary - proj.v2.Index(axis)) * -facing
            If Single.IsNaN(p1) OrElse Single.IsNaN(p2) Then
                side1 = False
            ElseIf Math.Sign(p1) <> Math.Sign(p2) AndAlso p1 <> 0 AndAlso p2 <> 0 Then 'must cross
                t = (p1 * -facing) / ray1.dir.W
                If t > ray1.min AndAlso t < ray1.max Then 'valid cross point
                    If p1 < 0 Then 'p1 on wrong side of plane
                        ray1.min = t
                    Else 'p2 on wrong side of plane
                        ray1.max = t
                    End If
                    side1 = True
                    collision = True

                Else 'entire ray behind plane
                    side1 = False
                End If
            ElseIf p1 < 0 AndAlso p2 < 0 Then 'entire ray behind plane
                side1 = False
            Else 'entire ray in front of plane 
                side1 = True
            End If


            'second side
            p1 = (boundary - proj.v2.Index(axis)) * -facing 'where +ve indicates it is on the side that the plane is facing (so a valid point)
            p2 = (boundary - proj.v3.Index(axis)) * -facing
            If Single.IsNaN(p1) OrElse Single.IsNaN(p2) Then
                side2 = False
            ElseIf Math.Sign(p1) <> Math.Sign(p2) AndAlso p1 <> 0 AndAlso p2 <> 0 Then 'must cross
                t = (p1 * -facing) / ray2.dir.W
                If t > ray2.min AndAlso t < ray2.max Then 'valid cross point
                    If p1 < 0 Then 'p1 on wrong side of plane
                        ray2.min = t
                    Else 'p2 on wrong side of plane
                        ray2.max = t
                    End If
                    side2 = True
                    collision = True

                Else 'entire ray behind plane
                    side2 = False
                End If
            ElseIf p1 < 0 AndAlso p2 < 0 Then 'entire ray behind plane
                side2 = False
            Else 'entire ray in front of plane 
                side2 = True
            End If


            'third side
            p1 = (boundary - proj.v3.Index(axis)) * -facing 'where +ve indicates it is on the side that the plane is facing (so a valid point)
            p2 = (boundary - proj.v1.Index(axis)) * -facing
            If Single.IsNaN(p1) OrElse Single.IsNaN(p2) Then
                side3 = False
            ElseIf Math.Sign(p1) <> Math.Sign(p2) AndAlso p1 <> 0 AndAlso p2 <> 0 Then 'must cross
                t = (p1 * -facing) / ray3.dir.W
                If t > ray3.min AndAlso t < ray3.max Then 'valid cross point
                    If p1 < 0 Then 'p1 on wrong side of plane
                        ray3.min = t
                    Else 'p2 on wrong side of plane
                        ray3.max = t
                    End If
                    side3 = True
                    collision = True

                Else 'entire ray behind plane
                    side3 = False
                End If
            ElseIf p1 < 0 AndAlso p2 < 0 Then 'entire ray behind plane
                side3 = False
            Else 'entire ray in front of plane 
                side3 = True
            End If


            'correct the triangles
            If collision Then
                Dim texCopy = tex 'copy needed for this, since originals not stored anywhere else (will be overwriting tex)
                'copy of the world coords not needed since they are stored in the ray structures

                If side1 Then 'add both points from the first ray
                    world.v1 = ray1.origin + (ray1.dir * ray1.min)
                    world.v2 = ray1.origin + (ray1.dir * ray1.max)
                    tex.t1 = texCopy.t1 + ((texCopy.t2 - texCopy.t1) * ray1.min)
                    tex.t2 = texCopy.t1 + ((texCopy.t2 - texCopy.t1) * ray1.max)
                End If

                If side2 Then
                    If side1 Then 'some points have already been added from side1
                        If ray1.max = 1 Then 'avoid duplicate points (ray1.max = ray2.min)
                            world.v3 = ray2.origin + (ray2.dir * ray2.max)
                            tex.t3 = texCopy.t2 + ((texCopy.t3 - texCopy.t2) * ray2.max)
                        Else
                            'add both points
                            world.v3 = ray2.origin + (ray2.dir * ray2.min)
                            tex.t3 = texCopy.t2 + ((texCopy.t3 - texCopy.t2) * ray2.min)

                            secondW.v2 = ray2.origin + (ray2.dir * ray2.max) 'add to second tri as well
                            secondTex.t2 = texCopy.t2 + ((texCopy.t3 - texCopy.t2) * ray2.max)
                            addTri = True
                        End If
                    Else
                        'add both points (since r1 is invalid)
                        world.v1 = ray2.origin + (ray2.dir * ray2.min)
                        world.v2 = ray2.origin + (ray2.dir * ray2.max)
                        tex.t1 = texCopy.t2 + ((texCopy.t3 - texCopy.t2) * ray2.min)
                        tex.t2 = texCopy.t2 + ((texCopy.t3 - texCopy.t2) * ray2.max)
                    End If
                End If

                'will only ever add 3rd or 4th vertex, since either of the other sides will contribute
                Dim canMin As Boolean = True
                Dim canMax As Boolean = True
                Dim newPoint As Boolean = False
                If side3 Then
                    If side1 Then
                        If ray1.min = 0 Then canMax = False 'ray1 meets side3, already added
                    End If
                    If side2 Then
                        If ray2.max = 1 Then canMin = False 'ray2 meets side3, already added
                    End If
                    If side1 AndAlso side2 Then 'two rays intersect and one is regular, so 4 points needed (may need to add the 4th here if it is this side that intersects one)
                        newPoint = True
                    End If

                    If canMax Then
                        If newPoint Then
                            secondW.v2 = ray3.origin + (ray3.dir * ray3.max) 'add to second tri
                            secondTex.t2 = texCopy.t3 + ((texCopy.t1 - texCopy.t3) * ray3.max)
                            addTri = True
                        Else
                            world.v3 = ray3.origin + (ray3.dir * ray3.max)
                            tex.t3 = texCopy.t3 + ((texCopy.t1 - texCopy.t3) * ray3.max)
                        End If
                    ElseIf canMin Then
                        If newPoint Then
                            secondW.v2 = ray3.origin + (ray3.dir * ray3.min) 'add to second tri
                            secondTex.t2 = texCopy.t3 + ((texCopy.t1 - texCopy.t3) * ray3.min)
                            addTri = True
                        Else
                            world.v3 = ray3.origin + (ray3.dir * ray3.min) 'add to second tri
                            tex.t3 = texCopy.t3 + ((texCopy.t1 - texCopy.t3) * ray3.min)
                        End If
                    End If
                End If
            End If

            If Not collision Then
                Exit Sub
            End If

            'transform new points
            Dim depth4 As Single
            world.v1.W = 1
            world.v2.W = 1
            world.v3.W = 1

            Dim depth1 As Single
            Dim depth2 As Single
            Dim depth3 As Single

            MatrixOp.Multiply(world.v1, tr, proj.v1)
            MatrixOp.Multiply(world.v2, tr, proj.v2)
            MatrixOp.Multiply(world.v3, tr, proj.v3)
            depth1 = proj.v1.W 'keep w for texture perspective correction later
            depth2 = proj.v2.W
            depth3 = proj.v3.W

            'perspective divide
            proj.v1 /= proj.v1.W
            proj.v2 /= proj.v2.W
            proj.v3 /= proj.v3.W

            proj.v1.W = depth1
            proj.v2.W = depth2
            proj.v3.W = depth3


            If addTri Then
                secondW.v2.W = 1
                MatrixOp.Multiply(secondW.v2, tr, secondP.v2)
                depth4 = secondP.v2.W
                secondP.v2 /= secondP.v2.W
                secondP.v2.W = depth4
            End If

            'second backface cull (on corrected triangles now)
            Dim ab As Vector4 = proj.v2 - proj.v1
            Dim ac As Vector4 = proj.v3 - proj.v1
            Dim sign As Single = (ab.X * ac.Y) - (ac.X * ab.Y)
            If sign * cull < 0 Then
                Exit Sub
            End If
        End If

        'distribute triangle
        'separating into strips by line (separate by y axis)
        Dim tCount As Integer = threadLocks.Length
        For t = 0 To tCount - 1
            'get bounds
            Dim upper = xBoundary * t
            Dim lower = xBoundary * (t + 1)
            If t = tCount - 1 Then lower = 2 'ensure the entire screen is filled (regardless of precision)

            If (proj.v1.X <= 1 - upper OrElse proj.v2.X <= 1 - upper OrElse proj.v3.X <= 1 - upper) AndAlso (proj.v1.X >= 1 - lower OrElse proj.v2.X >= 1 - lower OrElse proj.v3.X >= 1 - lower) Then 'within this portion
                threadVBuffers(t).Add(proj)
                threadTBuffers(t).Add(tex)
                If ((threadTBuffers(t).Count - 1) - stopPos(t) > size) OrElse stopPos(t) = 0 Then 'wake up render thread; start immediately at beginning to hide the cost of this code
                    stopPos(t) = threadTBuffers(t).Count - 1
                    threadLocks(t).Set()
                End If
            End If
        Next

        If addTri Then 'also call for any triangles created during the clipping process
            'fully set up second triangle
            secondW.v1 = world.v3
            secondW.v3 = world.v1
            secondTex.t1 = tex.t3
            secondTex.t3 = tex.t1
            secondP.v1 = proj.v3
            secondP.v3 = proj.v1

            'distribute triangle
            For t = 0 To tCount - 1
                'get bounds
                Dim upper = xBoundary * t
                Dim lower = xBoundary * (t + 1)
                If t = tCount - 1 Then lower = 2 'ensure the entire screen is filled (regardless of precision)

                If (secondP.v1.X <= 1 - upper OrElse secondP.v2.X <= 1 - upper OrElse secondP.v3.X <= 1 - upper) AndAlso (secondP.v1.X >= 1 - lower OrElse secondP.v2.X >= 1 - lower OrElse secondP.v3.X >= 1 - lower) Then 'within this portion
                    threadVBuffers(t).Add(secondP)
                    threadTBuffers(t).Add(secondTex)
                    If ((threadTBuffers(t).Count - 1) - stopPos(t) > size) OrElse stopPos(t) = 0 Then 'wake up render thread; start immediately at beginning to hide the cost of this code
                        stopPos(t) = threadTBuffers(t).Count - 1
                        threadLocks(t).Set()
                    End If
                End If
            Next
        End If
    End Sub

    <MethodImpl(MethodImplOptions.AggressiveOptimization)>
    Private Sub TransformTriangles(tr As Transform)
        If temp Is Nothing OrElse temp.Length < obj.contents.vertex.Length - 1 Then ReDim temp(obj.contents.vertex.Length - 1)

        Dim ref = obj.contents.vertex
        Parallel.For(0, ref.Length,
            Sub(i)
                Dim v = New Vector4(ref(i), 1)

                MatrixOp.Multiply(v, tr, v)

                Dim d = 1 / v.W
                v.X *= d
                v.Y *= d
                v.Z *= d

                temp(i) = v
            End Sub)
    End Sub

    'running for each render thread active
    <MethodImpl(MethodImplOptions.AggressiveOptimization)>
    Private Sub RenderTriangles(thread As Integer)
        Dim xBoundary = 2 / threadLocks.Length 'marks the amount of screen space for each thread
        While Not stopThreads 'while rendering is active
            'set the thread locks to avoid race conditions or deadlock, and ensure all triangles will be rendered
            main(thread).Set()
            threadLocks(thread).Wait()
            main(thread).Reset()

            While current(thread) < stopPos(thread) 'if geometry thread updates in meantime, then make sure to continue updating; assumed rendering to same buffer until wait at threadlock
                Dim l As Integer

                'get bounds for screen rendering (divide xBoundary by 2 to get value for screen portion, not ndc space
                Dim threadIndex As Integer = (threadLocks.Length - thread) - 1 'if thread 0, then threadIndex = 15 (flip sides)
                Dim upper As Integer = ((xBoundary / 2) * threadIndex) * cam.resX
                Dim lower As Integer = ((xBoundary / 2) * (threadIndex + 1)) * cam.resX
                If thread = 0 Then lower = cam.resX
                If current(thread) <> 0 Then current(thread) += 1 'handle special case and avoid duplicate triangles from l-1

                For l = current(thread) To stopPos(thread) 'where l represents the index for the current triangle, which is held within threadVBuffers 
                    Dim current As Integer = l

                    'run code
                    Dim opt As New Options
                    Dim sides1 As New Sides
                    Dim sides2 As New Sides
                    Dim tri As New Tri
                    Dim n As Single = cam.near

                    Dim rOptions = Me.rOptions(threadIndex)

                    Dim ab As Vector4
                    Dim ac As Vector4
                    Dim resX As Integer = cam.resX
                    Dim resY As Integer = cam.resY

                    Dim i1 As Integer = 0
                    Dim i2 As Integer = 1
                    Dim i3 As Integer = 2

                    Dim v1P As Vector4 = threadVBuffers(thread)(current).v1
                    Dim v2P As Vector4 = threadVBuffers(thread)(current).v2
                    Dim v3P As Vector4 = threadVBuffers(thread)(current).v3

                    Dim texture As Integer = Fix(threadTBuffers(thread)(current)(0).Z)
                    Dim tex = obj.contents.texVal(texture)

                    Dim transparency = obj.contents.transparency(texture)

                    'dimension variables for drawing triangles (don't set them until all checks are complete)
                    Dim bc As Vector4

                    Dim splitX As Single
                    Dim splitY As Single

                    Dim invSlopeTop1 As Single
                    Dim invSlopeTop2 As Single

                    Dim invSlopeBottom1 As Single
                    Dim invSlopeBottom2 As Single

                    Dim currentX1 As Single
                    Dim currentX2 As Single
                    Dim currentY1 As Single
                    Dim currentY2 As Single

                    Dim stepD As Integer

                    Dim rZ1 As Single
                    Dim rZ2 As Single
                    Dim rZ3 As Single

                    Dim multTex1 As Vector2
                    Dim multTex2 As Vector2
                    Dim multTex3 As Vector2
                    Dim texWidth As Integer

                    Dim currentTexture() As Integer

                    'get world positions for interpolation before vectors are swapped around
                    Dim w1 As Vector4
                    Dim w2 As Vector4
                    Dim w3 As Vector4
                    If rOptions.lighting Then
                        w1 = v1P * v1P.W
                        w2 = v2P * v2P.W
                        w3 = v3P * v3P.W
                        w1.W = v1P.W
                        w2.W = v2P.W
                        w3.W = v3P.W
                        MatrixOp.Multiply(w1, inv, w1)
                        MatrixOp.Multiply(w2, inv, w2)
                        MatrixOp.Multiply(w3, inv, w3)

                        tri.toInterpolate.l1 = w1
                        tri.toInterpolate.l2 = w2
                        tri.toInterpolate.l3 = w3
                        Dim w As New Vector3(w1.X, w1.Y, w1.Z)
                        tri.normal = Vector3.Normalize(Vector3.Cross(New Vector3(w2.X, w2.Y, w2.Z) - w, New Vector3(w3.X, w3.Y, w3.Z) - w))

                        w1 = w1 * (1 / v1P.W)
                        w2 = w2 * (1 / v2P.W)
                        w3 = w3 * (1 / v3P.W)
                    End If

                    'could perhaps perform this sorting in the geometry thread
                    If 1 - v1P.Y > 1 - v2P.Y Then
                        Dim temp As Vector4 = v1P
                        v1P = v2P
                        v2P = temp
                        Dim tempI = i1
                        i1 = i2
                        i2 = tempI
                        Dim tempW = w1
                        w1 = w2
                        w2 = tempW
                    End If
                    If 1 - v2P.Y > 1 - v3P.Y Then
                        Dim temp As Vector4 = v3P
                        v3P = v2P
                        v2P = temp
                        Dim tempI = i3
                        i3 = i2
                        i2 = tempI
                        Dim tempW = w3
                        w3 = w2
                        w2 = tempW
                    End If
                    If 1 - v1P.Y > 1 - v2P.Y Then
                        Dim temp As Vector4 = v1P
                        v1P = v2P
                        v2P = temp
                        Dim tempI = i1
                        i1 = i2
                        i2 = tempI
                        Dim tempW = w1
                        w1 = w2
                        w2 = tempW
                    End If

                    tri.toInterpolate.v1 = w1
                    tri.toInterpolate.v2 = w2
                    tri.toInterpolate.v3 = w3

                    'to screen space
                    'scale; + 1 to convert coords to [0,2], halve to get [0,1] and then scale by resX and resY. 1- for resY to flip image correctly
                    v1P.X = Fix(((v1P.X + 1) * 0.5 * resX) + 0.5F) 'add 0.5 to the number so that Fix() will actually end up rounding the number (avoid gaps); Fix ~ 4-5x faster than math.round
                    v1P.Y = Fix(((1 - v1P.Y) * 0.5 * resY) + 0.5F) 'get rid of the v1.Y + 1, to ensure that the fov scales evenly

                    v2P.X = Fix(((v2P.X + 1) * 0.5 * resX) + 0.5F)
                    v2P.Y = Fix(((1 - v2P.Y) * 0.5 * resY) + 0.5F)

                    v3P.X = Fix(((v3P.X + 1) * 0.5 * resX) + 0.5F)
                    v3P.Y = Fix(((1 - v3P.Y) * 0.5 * resY) + 0.5F)

                    'multiply out textures
                    Dim size = obj.contents.texSize(texture)
                    Dim tempU1 As Single = threadTBuffers(thread)(current)(i1).X * size.X
                    Dim tempV1 As Single = (1 - threadTBuffers(thread)(current)(i1).Y) * size.Y '1 - tBuffer because the range (from 0 to 1) assumes 0 as being at the end of the array rather than the front (for the y axis)

                    Dim tempU2 As Single = threadTBuffers(thread)(current)(i2).X * size.X
                    Dim tempV2 As Single = (1 - threadTBuffers(thread)(current)(i2).Y) * size.Y

                    Dim tempU3 As Single = threadTBuffers(thread)(current)(i3).X * size.X
                    Dim tempV3 As Single = (1 - threadTBuffers(thread)(current)(i3).Y) * size.Y

                    'needs to be done before vertices are ordered
                    Dim t1 = New Vector2(tempU1, tempV1)
                    Dim t2 = New Vector2(tempU2, tempV2)
                    Dim t3 = New Vector2(tempU3, tempV3)

                    tri.v1 = v1P
                    tri.v2 = v2P
                    tri.v3 = v3P

                    'perform checks
                    Dim check1 As Boolean = Not ((v1P.Y < 0 AndAlso v2P.Y < 0) OrElse (v1P.Y > resY - 1 AndAlso v2P.Y > resY - 1)) 'top tri y check
                    Dim check2 As Boolean = Not ((v3P.Y < 0 AndAlso v2P.Y < 0) OrElse (v3P.Y > resY - 1 AndAlso v2P.Y > resY - 1)) 'bottom tri y check

                    If check1 OrElse check2 Then
                        'Second: Determine x where triangle will be split, using intercept theorem
                        ab = v2P - v1P
                        ac = v3P - v1P
                        bc = v3P - v2P

                        splitX = v1P.X + (ab.Y / ac.Y) * ac.X
                        splitY = v2P.Y

                        'will iterate starting from v1 down
                        invSlopeTop1 = ab.X / ab.Y
                        invSlopeTop2 = (splitX - v1P.X) / (splitY - v1P.Y)

                        sides1.invSlope = invSlopeTop1
                        sides2.invSlope = invSlopeTop2

                        'will iterate starting from v3 up
                        invSlopeBottom1 = bc.X / bc.Y
                        invSlopeBottom2 = (v3P.X - splitX) / (v3P.Y - splitY)

                        currentX1 = v1P.X 'keep as single for precision, only convert to integer in for loop
                        currentX2 = v1P.X
                        currentY1 = v1P.Y
                        currentY2 = v2P.Y

                        stepD = 1

                        'caching data for perspective texture mapping 
                        rZ1 = 1 / v1P.W
                        rZ2 = 1 / v2P.W
                        rZ3 = 1 / v3P.W
                        tri.toInterpolate.b.rDeterminant = 1 / ((-bc.Y * -ac.X) + ((v3P.X - v2P.X) * -ac.Y))

                        tri.toInterpolate.b.part3 = -bc.Y
                        tri.toInterpolate.b.part4 = ac.Y

                        multTex1 = t1 * rZ1
                        multTex2 = t2 * rZ2
                        multTex3 = t3 * rZ3
                        texWidth = obj.contents.texSize(texture).X

                        currentTexture = obj.contents.texVal(texture)

                        tri.rZ1 = rZ1
                        tri.rZ2 = rZ2
                        tri.rZ3 = rZ3

                        tri.multTex1 = multTex1
                        tri.multTex2 = multTex2
                        tri.multTex3 = multTex3
                    End If

                    'top triangle //==================================================
                    'If both y values are above or under screen, then don't draw. Also the same for x
                    If check1 Then

                        'clamp values                             
                        currentY1 = Math.Clamp(currentY1, 0, resY - 1)
                        currentY2 = Math.Clamp(currentY2, 0, resY - 1)

                        'If either of the slopes are undefined, then don't draw triangle
                        If Not Single.IsNaN(invSlopeTop1) AndAlso Not Single.IsNaN(invSlopeTop2) Then

                            'Determine starting x values (bearing in mind they are coming from same y point)
                            If currentY1 <> v1P.Y Then
                                Dim change As Single = Math.Abs(v1P.Y - currentY1) * invSlopeTop1
                                currentX1 += change

                                change = Math.Abs(v1P.Y - currentY1) * invSlopeTop2
                                currentX2 += change
                            End If

                            'Determine step direction
                            If currentX1 + invSlopeTop1 > currentX2 + invSlopeTop2 Then
                                stepD = -1
                            End If

                            'clamp values
                            Dim xBound1 As Single = currentX1
                            Dim xBound2 As Single = currentX2

                            If currentX1 > lower - 1 Then
                                xBound1 = lower - 1
                            ElseIf currentX1 < upper Then
                                xBound1 = upper
                            End If
                            If currentX2 > lower - 1 Then
                                xBound2 = lower - 1
                            ElseIf currentX2 < upper Then
                                xBound2 = upper
                            End If

                            'Draw top triangle
                            Dim cY1 As Integer = Fix(currentY1)
                            Dim cY2 As Integer = Fix(currentY2)

                            tri.opt = opt
                            For r As Integer = cY1 To cY2
                                sides1.xB = xBound1
                                sides2.xB = xBound2

                                tri.opt.isTop = True
                                tri.opt.isParallel = False

                                tri.side1 = sides1
                                tri.side2 = sides2

                                FillTriangle(r, tri, stepD, rOptions, size.X, tex, transparency)

                                currentX1 += invSlopeTop1
                                currentX2 += invSlopeTop2

                                'clamp values
                                xBound1 = currentX1
                                xBound2 = currentX2
                                If currentX1 > lower - 1 Then
                                    xBound1 = lower - 1
                                ElseIf currentX1 < upper Then
                                    xBound1 = upper
                                End If
                                If currentX2 > lower - 1 Then
                                    xBound2 = lower - 1
                                ElseIf currentX2 < upper Then
                                    xBound2 = upper
                                End If
                            Next
                        End If
                    End If

                    'bottom triangle //================================================
                    'if either of the slopes are undefined, don't draw the triangle
                    If Not (Single.IsNaN(invSlopeBottom1) OrElse Single.IsNaN(invSlopeBottom2)) Then
                        currentX1 = v3P.X
                        currentX2 = v3P.X
                        currentY1 = v3P.Y
                        currentY2 = v2P.Y

                        'if both y values are above screen or below screen, don't draw. same for x
                        If check2 Then
                            sides1.invSlope = invSlopeBottom1
                            sides2.invSlope = invSlopeBottom2

                            'clamp values                          
                            currentY1 = Math.Clamp(currentY1, 0, resY - 1)
                            currentY2 = Math.Clamp(currentY2, 0, resY - 1)

                            'Determine starting x values
                            If currentY1 <> v3P.Y Then
                                Dim change As Single = Math.Abs(v3P.Y - currentY1) * invSlopeBottom1
                                currentX1 -= change

                                change = Math.Abs(v3P.Y - currentY1) * invSlopeBottom2
                                currentX2 -= change
                            End If

                            'Determine step direction
                            stepD = 1
                            If currentX1 - invSlopeBottom1 > currentX2 - invSlopeBottom2 Then
                                stepD = -1
                            End If

                            'clamp values
                            Dim xBound1 As Single = currentX1
                            Dim xBound2 As Single = currentX2

                            If currentX1 > lower - 1 Then
                                xBound1 = lower - 1
                            ElseIf currentX1 < upper Then
                                xBound1 = upper
                            End If
                            If currentX2 > lower - 1 Then
                                xBound2 = lower - 1
                            ElseIf currentX2 < upper Then
                                xBound2 = upper
                            End If

                            'Draw bottom triangle
                            Dim cY1 As Integer = Fix(currentY1)
                            Dim cY2 As Integer = Fix(currentY2)

                            tri.opt = opt
                            For r As Integer = cY1 To cY2 Step -1
                                sides1.xB = xBound1
                                sides2.xB = xBound2

                                tri.opt.isTop = False
                                tri.opt.isParallel = False

                                tri.side1 = sides1
                                tri.side2 = sides2

                                FillTriangle(r, tri, stepD, rOptions, size.X, tex, transparency)

                                currentX1 -= invSlopeBottom1
                                currentX2 -= invSlopeBottom2

                                'clamp values
                                xBound1 = currentX1
                                xBound2 = currentX2
                                If currentX1 > lower - 1 Then
                                    xBound1 = lower - 1
                                ElseIf currentX1 < upper Then
                                    xBound1 = upper
                                End If
                                If currentX2 > lower - 1 Then
                                    xBound2 = lower - 1
                                ElseIf currentX2 < upper Then
                                    xBound2 = upper
                                End If
                            Next
                        End If
                    End If
                Next
                Me.current(thread) = l - 1 'set = -1 to match stopPos correctly
            End While

            threadLocks(thread).Reset()
        End While
    End Sub


    <MethodImpl(MethodImplOptions.AggressiveOptimization)>
    Private Sub FillTriangle(r As Integer, ByRef tri As Tri, stepD As Integer, rOptions As RenderOptions, texWidth As Integer, currentTexture() As Integer, transparency As Single)
        Dim resX = cam.resX

        'getting some variables from the parameters
        Dim zBuffer() = cam.zBuffer
        Dim buffer() = cam.vBuffer

        Dim sides1 = tri.side1
        Dim sides2 = tri.side2
        Dim part3 = tri.toInterpolate.b.part3
        Dim part4 = tri.toInterpolate.b.part4
        Dim rDeterminant = tri.toInterpolate.b.rDeterminant
        Dim opt = tri.opt

        Dim isParallel = opt.isParallel
        Dim isTop = opt.isTop
        Dim cY1 = sides1.cY
        Dim cY2 = sides2.cY
        Dim currentX1 = sides1.currentX
        Dim currentX2 = sides2.currentX
        Dim invSlope1 = sides1.invSlope
        Dim invSlope2 = sides2.invSlope
        Dim xB1 = sides1.xB
        Dim xB2 = sides2.xB

        Dim v1 = tri.v1
        Dim v2 = tri.v2
        Dim v3 = tri.v3
        Dim rZ1 = tri.rZ1
        Dim rZ2 = tri.rZ2
        Dim rZ3 = tri.rZ3
        Dim multTex1 = tri.multTex1
        Dim multTex2 = tri.multTex2
        Dim multTex3 = tri.multTex3

        Dim n1 = tri.toInterpolate.v1
        Dim n2 = tri.toInterpolate.v2
        Dim n3 = tri.toInterpolate.v3

        Dim l1 = tri.toInterpolate.l1
        Dim l2 = tri.toInterpolate.l2
        Dim l3 = tri.toInterpolate.l3

        Dim lighting = lights
        Dim normal = tri.normal

        If xB1 <> xB2 Then
            'cache more terms of the barycentric coordinates that will be used for texture mapping
            Dim part1 As Single = ((v3.X - v2.X) * (r - v3.Y))
            Dim part2 As Single = ((v1.X - v3.X) * (r - v3.Y))

            'interpolation
            Dim firstValid As Integer = -1
            Dim lastValid As Integer = -1

            Dim b1a As Single
            Dim b1b As Single
            Dim b2a As Single
            Dim b2b As Single
            Dim b3a As Single
            Dim b3b As Single

            'find first valid point
            Dim xIB1 As Integer = Fix(xB1 + 0.5F)
            Dim xIB2 As Integer = Fix(xB2 + 0.5F)
            For p As Integer = xIB1 To xIB2 Step stepD
                Dim part5 As Single = p - v3.X

                b1a = ((part3 * part5) + part1) * rDeterminant
                b2a = ((part4 * part5) + part2) * rDeterminant
                b3a = 1 - (b1a + b2a)

                If b1a >= 0 AndAlso b2a >= 0 AndAlso b1a + b2a <= 1 Then 'valid point in triangle (when accounting for rounding issues)
                    firstValid = p
                    Exit For
                End If
            Next

            'find last valid point
            For p As Integer = xIB2 To xIB1 Step -stepD
                Dim part5 As Single = p - v3.X

                b1b = ((part3 * part5) + part1) * rDeterminant
                b2b = ((part4 * part5) + part2) * rDeterminant
                b3b = 1 - (b1b + b2b)

                If b1b >= 0 AndAlso b2b >= 0 AndAlso b1b + b2b <= 1 Then 'valid point in triangle (when accounting for rounding issues)
                    lastValid = p
                    Exit For
                End If
            Next

            'Interpolate
            If firstValid <> -1 Then
                Dim diffX As Integer = lastValid - firstValid
                Dim recDiffX As Single = 1 / diffX

                'interpolation for ndc space points
                Dim va = (n1 * b1a) + (n2 * b2a) + (n3 * b3a)
                Dim vb = (n1 * b1b) + (n2 * b2b) + (n3 * b3b)
                Dim diffA = ((vb - va) * recDiffX) * stepD

                'depth
                Dim depthA As Single = ((b1a * v1.Z) + (b2a * v2.Z) + (b3a * v3.Z))
                Dim depthB As Single = ((b1b * v1.Z) + (b2b * v2.Z) + (b3b * v3.Z))

                Dim startD As Single = depthA
                Dim diffD As Single = ((depthB - depthA) * recDiffX) * stepD

                'texture coords
                Dim startTX As Single = ((b1a * multTex1.X) + (b2a * multTex2.X) + (b3a * multTex3.X))
                Dim startTY As Single = ((b1a * multTex1.Y) + (b2a * multTex2.Y) + (b3a * multTex3.Y))
                Dim txB As Single = ((b1b * multTex1.X) + (b2b * multTex2.X) + (b3b * multTex3.X))
                Dim tyB As Single = ((b1b * multTex1.Y) + (b2b * multTex2.Y) + (b3b * multTex3.Y))

                Dim diffTX As Single = ((txB - startTX) * recDiffX) * stepD
                Dim diffTY As Single = ((tyB - startTY) * recDiffX) * stepD

                'reciprocal depth
                Dim startRecDepth As Single = ((b1a * rZ1) + (b2a * rZ2) + (b3a * rZ3))
                Dim recDepthB As Single = ((b1b * rZ1) + (b2b * rZ2) + (b3b * rZ3))

                Dim diffRecDepth As Single = (recDepthB - startRecDepth) * recDiffX * stepD

                'caching other variables
                Dim rLength As Integer = r * resX

                Dim startP As Integer = firstValid + rLength

                Dim startZI As Integer = rLength + firstValid

                Dim cont As Boolean = True

                'loop between the valid points
                For p = firstValid To lastValid Step stepD
                    If rOptions.asWireFrame Then
                        If p <> firstValid AndAlso p <> lastValid Then
                            Continue For 'only draw wireframe
                        End If
                    End If

                    If rOptions.toZBuffer Then
                        cont = False
                        If rOptions.isSkybox Then 'not used
                            If zBuffer(startZI) = Single.MaxValue Then
                                cont = True
                            End If
                        Else
                            If Not depth0 Then
                                If startD < zBuffer(startZI) Then
                                    zBuffer(startZI) = startD
                                    cont = True
                                End If
                            ElseIf depth0 Then
                                If (-startD < zBuffer(startZI) And zBuffer(startZI) > 0) OrElse (-startD > zBuffer(startZI) And zBuffer(startZI) < 0) Then 'ensure z ordering for 3d ui elements
                                    zBuffer(startZI) = -startD
                                    cont = True
                                End If
                            End If
                        End If
                    End If

                    If cont Then
                        If rOptions.toVBuffer Then
                            Dim w As Single = 1 / startRecDepth

                            Dim newX As Integer = Fix(startTX * w)
                            Dim newY As Integer = Fix(startTY * w)

                            Dim index As Integer = (newY * texWidth) + newX

                            Dim tx = currentTexture(index)
                            Dim r255 As Single = 1.0F / 255.0F

                            Dim prev = buffer(startP)
                            Dim pr As New Vector4((prev And 255) * r255, ((prev And 65280) >> 8) * r255, ((prev And 16711680) >> 16) * r255, ((prev And 4278190080) >> 24) * r255)

                            Dim pbgr As New Vector4((tx And 255) * r255, ((tx And 65280) >> 8) * r255, ((tx And 16711680) >> 16) * r255, ((tx And 4278190080) >> 24) * r255)

                            Dim final = pbgr

                            If rOptions.lighting Then 'lighting calculations here
                                Dim wPix = va * w ' correctly interpolate the world coordinates

                                If Not depth0 AndAlso obj.lightRef = -1 Then
                                    'apply ambient lighting (held in main camera)
                                    Dim ambient As New Vector4((cam.ambient And 255) * r255, ((cam.ambient And 65280) >> 8) * r255, ((cam.ambient And 16711680) >> 16) * r255, 1)
                                    final *= ambient 'first step: apply ambience (to everything, not just stuff that's lit)                     

                                    For l = 0 To lights.Length - 1
                                        If lights(l).Range = 0 Then Continue For 'deleted light

                                        'index cubemap to get the correct face and then get this pixel's depth based on which face is chosen (since light faces down axis)
                                        Dim light = lighting(l)

                                        Dim lightPos As New Vector4(light.pos, 1)
                                        Dim lp = wPix - lightPos
                                        Dim alp = Vector4.Abs(lp)

                                        Dim uv As Vector2

                                        Dim ma As Single
                                        Dim face As Integer

                                        'based on the face, get the pixel depth here; may be able to use indexing to avoid if statements
                                        Dim t1 = l1 - lightPos
                                        Dim t2 = l2 - lightPos
                                        Dim t3 = l3 - lightPos

                                        Dim facing As Single
                                        Dim depthP As Single
                                        If alp.Z >= alp.X AndAlso alp.Z >= alp.Y Then 'facing along z axis (screen +-x +y)
                                            If lp.Z < 0 Then face = 0 : uv.X = lp.X Else face = 1 : uv.X = -lp.X
                                            ma = (1 / alp.Z) * 0.5F 'reciprocal of alpz * 0.5 is less expensive than 0.5/alpz
                                            depthP = alp.Z
                                            uv.Y = -lp.Y

                                            facing = (-face * 2) + 1
                                            Dim mult1 = 1 / t1.Z
                                            Dim mult2 = 1 / t2.Z
                                            Dim mult3 = 1 / t3.Z
                                            t1 *= mult1 * facing
                                            t2 *= mult2 * facing
                                            t3 *= mult3 * facing
                                        ElseIf alp.Y >= alp.X Then 'facing along y axis (screen +x +-z)
                                            If lp.Y < 0 Then face = 5 : uv.Y = lp.Z Else face = 4 : uv.Y = -lp.Z
                                            ma = (1 / alp.Y) * 0.5F
                                            depthP = alp.Y
                                            uv.X = lp.X

                                            facing = (-(face - 4) * 2) + 1
                                            Dim mult1 = 1 / t1.Y
                                            Dim mult2 = 1 / t2.Y
                                            Dim mult3 = 1 / t3.Y
                                            t1 *= mult1
                                            t2 *= mult2
                                            t3 *= mult3
                                            t1.Y = t1.Z 'set the y as needed
                                            t2.Y = t2.Z
                                            t3.Y = t3.Z
                                        Else 'facing along x axis (screen +-z -y)
                                            If lp.X < 0 Then face = 2 : uv.X = -lp.Z Else face = 3 : uv.X = lp.Z
                                            ma = (1 / alp.X) * 0.5F
                                            depthP = alp.X
                                            uv.Y = -lp.Y

                                            facing = -((-(face - 2) * 2) + 1)
                                            Dim mult1 = 1 / t1.X
                                            Dim mult2 = 1 / t2.X
                                            Dim mult3 = 1 / t3.X
                                            t1 *= mult1
                                            t2 *= mult2
                                            t3 *= mult3
                                            t1.X = t1.Z 'set the x as needed
                                            t2.X = t2.Z
                                            t3.X = t3.Z
                                        End If

                                        'determine culling according to the light (assumes user has back-face culling)
                                        Dim noLighting As Boolean = False
                                        Dim ab = t2 - t1
                                        Dim ac = t3 - t1
                                        Dim sign As Single = (ab.X * ac.Y) - (ac.X * ab.Y)
                                        If sign * facing <= 0 Then 'either front or back face culls according to map chosen (assumes light has front-faced culling)
                                            noLighting = True
                                        End If

                                        uv *= ma 'vector normalised here
                                        uv += New Vector2(0.5F)

                                        'scale to resolution
                                        uv *= rOptions.shadowRes - 1
                                        Dim x As Integer = Fix(uv.X + 0.5F)
                                        Dim y As Integer = Fix(uv.Y + 0.5F)
                                        Dim depth As Single = light.cams(face).zBuffer((rOptions.shadowRes * y) + x)

                                        'scale depthP to match light depth scaling
                                        depthP = ((-depthP * light.cams(face).zTransform.X) + (light.cams(face).zTransform.Y)) * (ma * 2)

                                        Dim diffuse As New Vector4((light.diffuse And 255) * r255, ((light.diffuse And 65280) >> 8) * r255, ((light.diffuse And 16711680) >> 16) * r255, 1)

                                        If Not noLighting AndAlso depthP <= depth Then
                                            'perform lighting here
                                            Dim d = lp.Length
                                            If d > light.Range Then 'cap light to range
                                                Continue For
                                            End If
                                            Dim wN = Vector3.Normalize(New Vector3(lp.X, lp.Y, lp.Z)) 'may be able to divide by d
                                            Dim amount = Vector3.Dot(wN, normal)

                                            final -= amount * pbgr * diffuse 'add light intensity
                                            final /= light.constModifier + (light.linModifier * d) + (light.expModifier * (d * d)) 'apply attenuation (falloff factor)

                                            'clamp value to ambient (stop any inverse lighting)
                                            Dim min = pbgr * ambient
                                            final = Vector4.Clamp(final, min, New Vector4(Single.MaxValue))
                                        End If
                                    Next
                                End If
                            End If

                            'transparency if needed
                            If rOptions.transparency = 1 AndAlso transparency <> 1 Then
                                final = (pr * (1 - transparency)) + (final * transparency) 'blending operation
                            End If

                            'combine back into one integer
                            final = New Vector4(Math.Clamp(final.X, 0F, 1.0F), Math.Clamp(final.Y, 0F, 1.0F), Math.Clamp(final.Z, 0F, 1.0F), Math.Clamp(final.W, 0F, 1.0F))
                            final *= 255 'restore full range of color
                            Dim cb As Integer = Fix(final.X + 0.5F)
                            Dim cg As Integer = Fix(final.Y + 0.5F)
                            Dim cr As Integer = Fix(final.Z + 0.5F)
                            Dim ca As Integer = Fix(final.W + 0.5F) '255
                            tx = cb Or cg << 8 Or cr << 16 Or ca << 24

                            buffer(startP) = tx
                        End If
                    End If

                    startD += diffD
                    startP += stepD
                    startZI += stepD

                    startTX += diffTX
                    startTY += diffTY
                    startRecDepth += diffRecDepth

                    va += diffA
                Next
            End If
        End If
    End Sub

    'for debugging purposes
    Public Sub DrawLine(cam As Camera, p1 As Vector3, p2 As Vector3, tr As Transform, color As Integer, opt As RenderOptions)
        If cam.vBuffer Is Nothing OrElse cam.vBuffer.Length <> cam.resX * cam.resY Then InitialiseBuffers(cam, cam.resX, cam.resY, opt) 'rescales buffers if needed        
        Dim t1 As New Vector4(p1, 1)
        Dim t2 As New Vector4(p2, 1)
        Dim resX = cam.resX
        Dim resY = cam.resY

        ProjMatrix(cam, tr)
        MatrixOp.Multiply(t1, tr, t1) 'project lines
        MatrixOp.Multiply(t2, tr, t2)
        If t1.W < cam.near OrElse t2.W < cam.near Then Exit Sub
        t1 /= t1.W
        t2 /= t2.W

        'scale
        t1.X = Fix(((t1.X + 1) * 0.5 * resX) + 0.5F)
        t1.Y = Fix(((1 - t1.Y) * 0.5 * resY) + 0.5F)

        t2.X = Fix(((t2.X + 1) * 0.5 * resX) + 0.5F)
        t2.Y = Fix(((1 - t2.Y) * 0.5 * resY) + 0.5F)

        If t1.X = t2.X AndAlso t1.Y = t2.Y Then 'draw single point if possible
            If t1.X >= 0 AndAlso t1.Y >= 0 AndAlso t1.X < resX AndAlso t1.Y < resY Then
                Dim ind As Integer = (t1.Y * resX) + t1.X
                If t1.Z < cam.zBuffer(ind) Then
                    cam.zBuffer(ind) = t1.Z
                    cam.vBuffer(ind) = color 'color white
                End If
            End If
            Exit Sub
        End If

        'special case to check: if they have the same x (horizontal line)
        'determine whether to loop through x or y here
        Dim change = t2 - t1
        Dim index As Integer
        If t1.X = t2.X Then
            index = 1
        End If
        Dim diff = t2.Index(index) - t1.Index(index)
        change /= Math.Abs(diff)

        If diff = 0 Then
            Exit Sub
        End If

        Dim count As Integer = 0

        If Single.IsNaN(t1.X) OrElse Single.IsNaN(t1.Y) OrElse Single.IsNaN(t1.Z) OrElse Single.IsNaN(t1.W) Then
            Exit Sub
        End If
        If Single.IsNaN(t2.X) OrElse Single.IsNaN(t2.Y) OrElse Single.IsNaN(t2.Z) OrElse Single.IsNaN(t2.W) Then
            Exit Sub
        End If

        For i = t1.Index(index) To t2.Index(index) Step Math.Sign(diff)
            count += 1
            Dim x As Integer
            Dim y As Integer
            If index = 0 Then 'get coordinates
                x = i : y = t1.Y + (change.Y * count)
            Else
                y = i : x = t1.X + (change.X * count)
            End If
            Dim z As Single = t1.Z + (change.Z * count)

            If x < 0 OrElse x > resX - 1 OrElse y < 0 OrElse y > resY - 1 Then 'clamp
                Continue For
            End If

            If index = t1.Index(index) OrElse index = t2.Index(index) Then
                Console.WriteLine()
            End If

            Dim ind As Integer = (y * resX) + x
            If z < cam.zBuffer(ind) Then
                cam.zBuffer(ind) = z
                cam.vBuffer(ind) = color 'color white
            End If
        Next
    End Sub

    'rotates and translates by camera, scales for projection
    Shared Function ProjMatrix(cam As Camera, ByRef tr As Transform) As Vector2
        'z' remapped between [0,1] after perspective divide => 0 when on near plane, 1 when on far plane; undefined for other values
        'projection window between [-1,1]
        'assuming when in camera space, camera is looking down -ve z axis (ie. pos x,y,-n will be on the near plane)
        Dim camDir = cam.dir
        Dim camPos = cam.pos
        Dim n = cam.near
        Dim zfar = cam.far

        'to camera space
        Dim sinX As Single = Math.Sin(camDir.X)
        Dim cosX As Single = Math.Cos(camDir.X)

        Dim sinY As Single = Math.Sin(camDir.Y)
        Dim cosY As Single = Math.Cos(camDir.Y)

        Dim sinZ As Single = Math.Sin(camDir.Z)
        Dim cosZ As Single = Math.Cos(camDir.Z)

        'composite transformations in form: rotX * rotY * rotZ * translation (first one to do goes last). Apply rotation in the opposite order to how the camera would rotate (to result in looking straight down z axis)
        Dim tTr As New Transform With {
            .x = New Vector4(1, 0, 0, 0),
            .y = New Vector4(0, 1, 0, 0),
            .z = New Vector4(0, 0, 1, 0),
            .w = New Vector4(-camPos.X, -camPos.Y, -camPos.Z, 1)}

        Dim xTr As New Transform With {
            .x = New Vector4(1, 0, 0, 0),
            .y = New Vector4(0, cosX, sinX, 0),
            .z = New Vector4(0, -sinX, cosX, 0),
            .w = New Vector4(0, 0, 0, 1)}

        Dim yTr As New Transform With {
            .x = New Vector4(cosY, 0, sinY, 0),
            .y = New Vector4(0, 1, 0, 0),
            .z = New Vector4(-sinY, 0, cosY, 0),
            .w = New Vector4(0, 0, 0, 1)}

        Dim zTr As New Transform With {
            .x = New Vector4(cosZ, sinZ, 0, 0),
            .y = New Vector4(-sinZ, cosZ, 0, 0),
            .z = New Vector4(0, 0, 1, 0),
            .w = New Vector4(0, 0, 0, 1)}

        'set up proj matrix
        Dim fov = cam.fov * (Math.PI / 180) 'to radians
        Dim aspect = cam.resX / cam.resY
        Dim f As Single = 1 / Math.Tan(fov / 2) '1/tan(90/2)=1 => no scaling at 90 fov
        Dim fa As Single = f * aspect
        Dim a = -zfar / (zfar - n)
        Dim b = -(zfar * n) / (zfar - n)

        Dim pTr As New Transform With {
            .x = New Vector4(f, 0, 0, 0),
            .y = New Vector4(0, fa, 0, 0),
            .z = New Vector4(0, 0, a, b),
            .w = New Vector4(0, 0, -1, 0)} 'this just makes w -z 

        'build transformation + proj matrix
        MatrixOp.Multiply(zTr, yTr, yTr)
        MatrixOp.Multiply(yTr, xTr, xTr)
        MatrixOp.Multiply(xTr, pTr, pTr)

        MatrixOp.Transpose(pTr, pTr)
        MatrixOp.Multiply(pTr, tTr, tr)
        Return New Vector2(a, b)
    End Function
    Shared Sub TransformMatrix(scale As Vector3, rot As Vector3, translation As Vector3, ByRef tr As Transform)
        Dim sinX As Single = Math.Sin(rot.X)
        Dim cosX As Single = Math.Cos(rot.X)

        Dim sinY As Single = Math.Sin(rot.Y)
        Dim cosY As Single = Math.Cos(rot.Y)

        Dim sinZ As Single = Math.Sin(rot.Z)
        Dim cosZ As Single = Math.Cos(rot.Z)

        'rotation
        Dim xTr As New Transform With {
            .x = New Vector4(1, 0, 0, 0),
            .y = New Vector4(0, cosX, sinX, 0),
            .z = New Vector4(0, -sinX, cosX, 0),
            .w = New Vector4(0, 0, 0, 1)}

        Dim yTr As New Transform With {
            .x = New Vector4(cosY, 0, -sinY, 0),
            .y = New Vector4(0, 1, 0, 0),
            .z = New Vector4(sinY, 0, cosY, 0),
            .w = New Vector4(0, 0, 0, 1)}

        Dim zTr As New Transform With {
            .x = New Vector4(cosZ, -sinZ, 0, 0),
            .y = New Vector4(sinZ, cosZ, 0, 0),
            .z = New Vector4(0, 0, 1, 0),
            .w = New Vector4(0, 0, 0, 1)}

        'scale
        Dim sTr As New Transform With {
            .x = New Vector4(scale.X, 0, 0, 0),
            .y = New Vector4(0, scale.Y, 0, 0),
            .z = New Vector4(0, 0, scale.Z, 0),
            .w = New Vector4(0, 0, 0, 1)}

        'translation
        Dim tTr As New Transform With {
            .x = New Vector4(1, 0, 0, 0),
            .y = New Vector4(0, 1, 0, 0),
            .z = New Vector4(0, 0, 1, 0),
            .w = New Vector4(-translation.X, -translation.Y, -translation.Z, 1)}

        'build transformation matrix
        MatrixOp.Transpose(tTr, tTr)
        MatrixOp.Multiply(zTr, tTr, tr)
        MatrixOp.Transpose(tr, tr)
        MatrixOp.Multiply(yTr, tr, tr)
        MatrixOp.Transpose(tr, tr)
        MatrixOp.Multiply(xTr, tr, tr)
        MatrixOp.Transpose(tr, tr)
        MatrixOp.Multiply(sTr, tr, tr)
    End Sub


    Public Sub InitialiseBuffers(cam As Camera, resX As Integer, resY As Integer, opt As RenderOptions)
        If opt.toVBuffer Then ReDim cam.vBuffer((resX * resY) - 1)
        If opt.toZBuffer Then ReDim cam.zBuffer((resX * resY) - 1)
        ClearBuffers(cam, opt)
    End Sub
    Private Sub ClearBuffers(cam As Camera, opt As RenderOptions)
        If opt.toVBuffer Then Array.Clear(cam.vBuffer)
        If opt.toZBuffer Then Array.Fill(cam.zBuffer, Single.MaxValue)
    End Sub
    Public Sub Display(bmp As WriteableBitmap, bPtr As IntPtr, cam As Camera, screen As Control)
        bmp.Dispatcher.Invoke(Sub()
                                  'resize bmp if necessary
                                  If bmp.PixelWidth <> cam.resX OrElse bmp.PixelHeight <> cam.resY OrElse screen.ActualHeight <> bmp.PixelHeight OrElse screen.ActualWidth <> bmp.PixelWidth Then bmp = New WriteableBitmap(cam.resX, cam.resY, 96, 96, bmp.Format, Nothing) : bPtr = bmp.BackBuffer : Exit Sub 'in case still resizing

                                  Marshal.Copy(cam.vBuffer, 0, bPtr, cam.vBuffer.Length)
                                  bmp.Lock()
                                  bmp.AddDirtyRect(New Int32Rect(0, 0, bmp.PixelWidth, bmp.PixelHeight))
                                  bmp.Unlock()
                                  screen.Background = New ImageBrush(bmp)
                              End Sub)
    End Sub
    Class MatrixOp
        '                                                         [a, a, a]
        'assuming rows multiplied by columns such order =  [a,b,c][b, b, b]
        '                                                         [c, c, c]
        'expects first (vector point) as row major order; second matrix in column major (cTr = column order transform matrix)
        <MethodImpl(MethodImplOptions.AggressiveInlining Or MethodImplOptions.AggressiveOptimization)>
        Shared Sub Multiply(inRow As Vector4, cTr As Pipeline.Transform, ByRef outVec As Vector4)
            outVec = New Vector4(Vector4.Dot(inRow, cTr.x), Vector4.Dot(inRow, cTr.y), Vector4.Dot(inRow, cTr.z), Vector4.Dot(inRow, cTr.w))
        End Sub

        'expects first matrix in row major order (each vector represents a row); second matrix in column major (each vector represents column); outputs row major matrix
        Shared Sub Multiply(rTr As Pipeline.Transform, cTr As Pipeline.Transform, ByRef out As Pipeline.Transform)
            Multiply(rTr.x, cTr, out.x)
            Multiply(rTr.y, cTr, out.y)
            Multiply(rTr.z, cTr, out.z)
            Multiply(rTr.w, cTr, out.w)
        End Sub

        'switches rows and columns
        Shared Sub Transpose(tr As Pipeline.Transform, ByRef out As Pipeline.Transform)
            out.x = New Vector4(tr.x.X, tr.y.X, tr.z.X, tr.w.X)
            out.y = New Vector4(tr.x.Y, tr.y.Y, tr.z.Y, tr.w.Y)
            out.z = New Vector4(tr.x.Z, tr.y.Z, tr.z.Z, tr.w.Z)
            out.w = New Vector4(tr.x.W, tr.y.W, tr.z.W, tr.w.W)
        End Sub

        'using gaussian elimination
        Shared Sub Inverse(tr As Pipeline.Transform, ByRef out As Pipeline.Transform)
            'set up identity matrix
            out.x = New Vector4(1, 0, 0, 0)
            out.y = New Vector4(0, 1, 0, 0)
            out.z = New Vector4(0, 0, 1, 0)
            out.w = New Vector4(0, 0, 0, 1)

            'clear x value (turns the first in x row to value 1 to match identity)
            out.x /= tr.x.X
            tr.x /= tr.x.X

            'clear the other column values in tr to 0 (so the first column will match identity), and repeat for the 'out' matrix (which will be the one returned as inverse)
            out.y -= out.x * tr.y.X
            tr.y -= tr.x * tr.y.X
            out.z -= out.x * tr.z.X
            tr.z -= tr.x * tr.z.X
            out.w -= out.x * tr.w.X
            tr.w -= tr.x * tr.w.X

            'clear y value (turns second in y row to value 1 to match identity)
            out.y /= tr.y.Y
            tr.y /= tr.y.Y

            'repeat the process, now for the second column (so the second value in the x, z, w rows will become 0 in tr)
            out.x -= out.y * tr.x.Y
            tr.x -= tr.y * tr.x.Y
            out.z -= out.y * tr.z.Y
            tr.z -= tr.y * tr.z.Y
            out.w -= out.y * tr.w.Y
            tr.w -= tr.y * tr.w.Y

            'clear z value (turns third in z row to value 1 to match identity)
            out.z /= tr.z.Z
            tr.z /= tr.z.Z

            'repeat for third column
            out.x -= out.z * tr.x.Z
            tr.x -= tr.z * tr.x.Z
            out.y -= out.z * tr.y.Z
            tr.y -= tr.z * tr.y.Z
            out.w -= out.z * tr.w.Z
            tr.w -= tr.z * tr.w.Z

            'clear last value (turns the fourth in w row to 1 to match identity)
            out.w /= tr.w.W
            tr.w /= tr.w.W

            'zero out the final values in the last column of tr
            out.x -= out.w * tr.x.W
            tr.x -= tr.w * tr.x.W
            out.y -= out.w * tr.y.W
            tr.y -= tr.w * tr.y.W
            out.z -= out.w * tr.z.W
            tr.z -= tr.w * tr.z.W
            'the out matrix now results in the inverse, where tr is now an identity matrix
        End Sub

        Shared Function ExtractRot(tr As Pipeline.Transform) As Vector3
            Dim x = 0F
            Dim y = 0F
            Dim z = 0F

            If tr.z.X < 1 Then
                If tr.z.X > -1 Then
                    y = Math.Asin(-tr.z.X)
                    z = -Math.Atan2(tr.y.X, tr.x.X)
                    x = Math.Atan2(tr.z.Y, tr.z.Z)
                Else 'if y rotation is exactly 90 degrees
                    y = Math.PI / 2
                    z = Math.Atan2(-tr.y.Z, tr.y.Y)
                    x = 0
                End If
            Else 'if y rotation is exactly -90 degrees
                y = -Math.PI / 2
                z = -Math.Atan2(-tr.y.Z, tr.y.Y)
                x = 0
            End If

            If Math.Atan2(tr.x.X, tr.z.Z) < 0 Then 'check if y rotation should be over 180 degrees or not (Math.Asin can only handle angles that are not reflex)
                y = Math.PI - y
            End If

            If Math.Abs(y) Mod 2 * Math.PI > Math.PI / 2 Then 'ensure correct x and z rotation if y rotation over 180 degrees
                z -= Math.PI
                x -= Math.PI
            End If

            Return New Vector3(x, y, z)
        End Function
    End Class
End Class


Public Class PointLight
    Public cams(5) As Camera
    Public pos As Vector3

    Public diffuse As Integer = BitConverter.ToInt32({255, 255, 255, 255})

    Public Range As Single

    Public constModifier As Single 'these three determine the light's falloff factor
    Public linModifier As Single
    Public expModifier As Single
End Class


Public Class Camera
    Public pos As Vector3
    Public dir As Vector3
    Public near As Single
    Public far As Single
    Public fov As Single

    Public vBuffer() As Integer
    Public zBuffer() As Single

    Public resX As Integer
    Public resY As Integer

    Public tr As Pipeline.Transform
    Public currentWDir As Vector3 'only used for certain cases like lighting
    Public zTransform As Vector2

    Public ambient As Integer = BitConverter.ToInt32({128, 128, 128, 255})
End Class


Public Class Model
    Public contents As Base

    Public rot As New Vector3
    Public scale As New Vector3(1)
    Public translation As New Vector3

    Public depth0 As Boolean = False
    Public depthToCentre As Single

    Public lightRef As Integer = -1 'reference to light list

    Public intersecting As Boolean = False
    Public index As BVH.Tuple2

    Public closest As Single 'closest point on the model's aabb from camera
End Class


Public Structure Ray3
    Dim origin As Vector3
    Dim dir As Vector3
    Dim min As Single
    Dim max As Single
End Structure
Public Structure Ray4
    Dim origin As Vector4
    Dim dir As Vector4
    Dim min As Single
    Dim max As Single
End Structure


Public Class Base
    Public filePath As String 'used to identify the base from file
    Public identifier As Integer 'used to find the base within array/list

    Public vertex() As Vector3
    Public index() As Tuple3

    Public texCoord() As Vector2
    Public texVal()() As Integer
    Public texSize() As Vector2
    Public transparency() As Single

    Public hasTransparent As Boolean = False 'if there are any transparent materials, this will be true

    'both bsp trees reference the same index array
    Public oBsp As BSPTree 'bsp tree works regardless of transformations, so can be put here
    Public tBsp As BSPTree 'this tree is only for transparent polygons

    'all 8 corners can be made from these, and transformed as needed by other classes
    Public lower As Vector3
    Public upper As Vector3

    Public Structure Tuple3
        Dim X As Integer
        Dim Y As Integer
        Dim Z As Integer
    End Structure

    'for preserving the file path only
    Sub New()

    End Sub

    Sub New(fPath As String, id As Integer, v() As Vector3, index() As Tuple3, texCoord() As Vector2, texVal()() As Integer, texSize() As Vector2, transparency() As Single, upper As Vector3, lower As Vector3)
        filePath = fPath
        identifier = id
        vertex = v
        Me.index = index
        Me.texCoord = texCoord
        Me.texVal = texVal
        Me.texSize = texSize
        Me.upper = upper
        Me.lower = lower
        Me.transparency = transparency

        'split the index list according to which parts of it have transparent triangles, and create two different bsp trees for them.
        'Can create and input the sort array that would be used in the constructor (since that defines what parts of the index buffer should be used - but both bsp trees reference same index buffer)
        Dim currentO As Integer
        Dim currentT As Integer
        For i = 0 To (index.Length \ 3) - 1
            If transparency(index(i * 3).Z) = 1 Then 'where index.z has the current texture index for this point; transparency value 1 = fully opaque
                currentO += 1
            Else
                hasTransparent = True
                currentT += 1
            End If
        Next

        Dim sortO(currentO - 1) As Integer
        Dim sortT(currentT - 1) As Integer
        currentO = 0
        currentT = 0
        For i = 0 To (index.Length \ 3) - 1
            If transparency(index(i * 3).Z) = 1 Then
                sortO(currentO) = i * 3
                currentO += 1
            Else
                sortT(currentT) = i * 3
                currentT += 1
            End If
        Next

        'create bsp trees
        If currentO > 0 Then
            oBsp = New BSPTree(sortO, vertex, index)
        End If
        If currentT > 0 Then
            tBsp = New BSPTree(sortT, vertex, index)
        End If
    End Sub


    Public Class BSPTree
        Public Class Node
            Public parent As Node
            Public self As Plane

            Public front As New List(Of Node)
            Public behind As New List(Of Node)

            'if the triangle is a duplicate, ensure it becomes a leaf (ie. do not split using the plane). Needed to prevent issues since duplicates reference the same class instance
            'also, only use the plane on a duplicate node (parent nor front nor behind will be the same for both instances)
            Public duplicate As Boolean
        End Class
        Public Class Plane
            Public normal As Vector3
            Public planeConstant As Single
            Public triangles As New List(Of Integer) 'reference to the index array for triangles on this plane
            Public self As Node
        End Class

        Private Const max_depth As Integer = 20
        Private Const min_leaf As Integer = 100
        Const epsilon As Single = 0.00001

        Public start As New Node
        Public sort() As Integer

        Dim vBuffer As Vector3()
        Dim iBuffer As Tuple3()

        'toSort defines the indices of the index buffer to use (e.g. [if index is 1 based] 1 = use index(1,2,3); so toSort(1,2,3) = use index(1,2,3,4,5,6,7,8,9); toSort(1,2,4) = use index(1,2,3,4,5,6,10,11,12) etc.)
        Public Sub New(toSort() As Integer, vertex() As Vector3, index() As Tuple3)
            start.self = New Plane
            sort = toSort
            vBuffer = vertex
            iBuffer = index

            Organise()
            Partition(start.front, 0)
            Partition(start.behind, 0)
        End Sub

        'each node after start will guaranteed be a unique node. only leaves can be duplicates
        Private Sub Organise()
            'calculate details for the start node
            start.self.triangles.Add(sort(0))
            start.self.self = start
            Dim startTri = start.self.triangles(0)

            Dim startV1 As Vector3 = vBuffer(iBuffer(startTri).X - 1)
            Dim startV2 As Vector3 = vBuffer(iBuffer(startTri + 1).X - 1)
            Dim startV3 As Vector3 = vBuffer(iBuffer(startTri + 2).X - 1)

            start.self.normal = Vector3.Normalize(Vector3.Cross(startV2 - startV1, startV3 - startV1))
            start.self.planeConstant = Vector3.Dot(start.self.normal, startV1)

            Dim startN As Vector3 = start.self.normal
            Dim startPConst As Single = start.self.planeConstant

            For i = 1 To sort.Length - 1
                Dim newV1 As Vector3 = vBuffer(iBuffer(sort(i)).X - 1)
                Dim newV2 As Vector3 = vBuffer(iBuffer(sort(i) + 1).X - 1)
                Dim newV3 As Vector3 = vBuffer(iBuffer(sort(i) + 2).X - 1)

                Dim newN As Vector3 = Vector3.Normalize(Vector3.Cross(newV2 - newV1, newV3 - newV1))
                Dim newPConst As Single = Vector3.Dot(newN, newV1)

                'determine which side of this plane the triangle is; if 0, then add it to this node
                Dim p1 As Single = Vector3.Dot(startN, newV1) - startPConst
                Dim p2 As Single = Vector3.Dot(startN, newV2) - startPConst
                Dim p3 As Single = Vector3.Dot(startN, newV3) - startPConst

                If p1 = 0 AndAlso p2 = 0 AndAlso p3 = 0 Then 'add to this node
                    start.self.triangles.Add(sort(i))
                    Continue For
                End If

                'use references to directly edit the list (since these are classes)
                'create new node and add it to the list
                Dim newNode As New Node With {.parent = start,
                    .self = New Plane With {.normal = newN, .planeConstant = newPConst}}
                newNode.self.triangles.Add(sort(i))
                newNode.self.self = newNode

                If p1 >= 0 AndAlso p2 >= 0 AndAlso p3 >= 0 Then
                    start.front.Add(newNode)

                ElseIf p1 <= 0 AndAlso p2 <= 0 AndAlso p3 <= 0 Then
                    start.behind.Add(newNode)

                Else 'crosses plane, check both lists for pre-existing planes
                    newNode.duplicate = True 'because references in both lists are the same, this node cannot be used as a parent node, and should not use the parent variable
                    start.front.Add(newNode)
                    start.behind.Add(newNode)

                End If
            Next
        End Sub

        'given a list of nodes (triangles) with precalculated planes, create a tree where nodes are ordered based on these planes
        Private Sub Partition(list As List(Of Node), depth As Integer)
            If depth > max_depth Then Exit Sub
            If list.Count <= min_leaf Then Exit Sub

            'take the first in list to sort (if is a duplicate triangle, then continue until non-duplicate is found or none left)
            Dim target As New Node
            Dim valid As Boolean = False

            'ensure that a duplicate cannot become a node
            Dim selected As Integer = 0 'list.Count / 2 'start from middle for more balanced approach
            While Not valid AndAlso selected < list.Count - 1
                If Not list(selected).duplicate Then
                    valid = True
                    target = list(selected)
                Else
                    selected += 1
                End If
            End While
            If Not valid Then selected = list.Count / 2
            While Not valid AndAlso selected > 0
                If Not list(selected).duplicate Then
                    valid = True
                    target = list(selected)
                Else
                    selected -= 1
                End If
            End While
            If Not valid Then Exit Sub

            'for each of the members being sorted, remove them from the 'list' list and add them to the current list based on position with plane
            Dim n As Vector3 = target.self.normal
            Dim pConst As Single = target.self.planeConstant
            Dim count As Integer = list.Count

            For i = 0 To count - 1
                If i = selected Then Continue For

                Dim newN As Node = list(i)
                Dim tri As Integer = newN.self.triangles(0)
                'calculate where each point of the current node triangle lies relative to the chosen plane (of the target node)
                Dim p1 As Single = Vector3.Dot(n, vBuffer(iBuffer(tri).X - 1)) - pConst
                Dim p2 As Single = Vector3.Dot(n, vBuffer(iBuffer(tri + 1).X - 1)) - pConst
                Dim p3 As Single = Vector3.Dot(n, vBuffer(iBuffer(tri + 2).X - 1)) - pConst

                If p1 = 0 AndAlso p2 = 0 AndAlso p3 = 0 Then 'lies on same plane
                    target.self.triangles.AddRange(newN.self.triangles)
                    Continue For
                End If

                newN.parent = target

                If p1 >= 0 AndAlso p2 >= 0 AndAlso p3 >= 0 Then 'lies in front
                    target.front.Add(newN)

                ElseIf p1 <= 0 AndAlso p2 <= 0 AndAlso p3 <= 0 Then 'lies behind
                    target.behind.Add(newN)

                Else 'crosses plane; add twice (resulting in a duplicate triangle)
                    newN.duplicate = True
                    target.front.Add(newN)
                    target.behind.Add(newN)
                End If
            Next

            'clear list so that only the target node is left
            list.Clear()
            list.Add(target)

            Partition(target.front, depth + 1) 'pass lists to sort by reference
            Partition(target.behind, depth + 1) 'using tail recursion which is more efficient (can be optimised better by the compiler)
        End Sub

        'returns true if there is an intersection (finds first intersection, not guaranteed to be closest)
        Public Function CheckIntersect(r As Ray3) As Boolean
            Dim tMin As Vector3 = r.origin + (r.min * r.dir)
            Dim tMax As Vector3 = r.origin + (r.max * r.dir)

            Return CheckPlane(r, tMin, tMax, start)
        End Function

        'random error when calling TestIntersect from this function sometimes
        Private Function CheckPlane(ByRef r As Ray3, ByRef tMin As Vector3, ByRef tMax As Vector3, ByRef current As Node) As Boolean
            Dim p1 As Single
            Dim p2 As Single

            'check if tMin and tMax cross the plane of the current node or not
            Dim n = current.self.normal
            Dim pConst = current.self.planeConstant
            p1 = (n.X * tMin.X + n.Y * tMin.Y + n.Z * tMin.Z) - pConst 'Vector3.Dot(current.self.normal, tMin) - pConst
            p2 = (n.X * tMax.X + n.Y * tMax.Y + n.Z * tMax.Z) - pConst 'Vector3.Dot(current.self.normal, tMax) - pConst

            Dim result As Boolean
            If p1 <> p2 AndAlso p1 <> 0 AndAlso p2 <> 0 Then 'crosses plane (test for intersections with triangle)
                result = TestIntersect(r, current)

                If result Then
                    Return True
                End If
            End If

            If current.front.Count > 0 AndAlso (p1 >= 0 OrElse p2 >= 0) Then 'check the front plane
                For i = 0 To current.front.Count - 1
                    Dim newC = current.front(i)
                    result = CheckPlane(r, tMin, tMax, newC)

                    If result Then
                        Return True
                    End If
                Next
            End If
            If current.behind.Count > 0 AndAlso (p1 <= 0 OrElse p2 <= 0) Then 'check behind plane
                For i = 0 To current.behind.Count - 1
                    Dim newC = current.behind(i)
                    result = CheckPlane(r, tMin, tMax, newC)

                    If result Then
                        Return True
                    End If
                Next
            End If

            Return False
        End Function

        Private Function TestIntersect(r As Ray3, currentNode As Node) As Boolean
            Dim tri As Integer
            Dim n As Vector3
            Dim pConst As Single
            Dim intersect As Vector3

            n = currentNode.self.normal
            pConst = currentNode.self.planeConstant
            tri = currentNode.self.triangles(0)

            'begin intersection tests
            Dim dot As Single = r.dir.X * n.X + r.dir.Y * n.Y + r.dir.Z * n.Z 'Vector3.Dot(r.dir, n)
            If dot > -epsilon AndAlso dot < epsilon Then 'very close to zero (within precision) --> parallel, so return
                Return False
            End If

            'Using P.N + d = 0
            Dim t As Single = -((r.origin.X * n.X + r.origin.Y * n.Y + r.origin.Z * n.Z) - pConst) / dot
            If t - epsilon < r.min OrElse t + epsilon > r.max Then
                Return False
            End If
            intersect = r.origin + (t * r.dir)

            'check for intersection, by translating triangle to origin and checking cross
            Dim p1 = vBuffer(iBuffer(tri).X - 1) - intersect
            Dim p2 = vBuffer(iBuffer(tri + 1).X - 1) - intersect
            Dim p3 = vBuffer(iBuffer(tri + 2).X - 1) - intersect

            Dim cross1 = New Vector3(p3.Y * p2.Z - p3.Z * p2.Y, p3.Z * p2.X - p3.X * p2.Z, p3.X * p2.Y - p3.Y * p2.X) 'Vector3.Cross(p3, p2)
            Dim cross2 = New Vector3(p3.Y * p1.Z - p3.Z * p1.Y, p3.Z * p1.X - p3.X * p1.Z, p3.X * p1.Y - p3.Y * p1.X) 'Vector3.Cross(p3, p1)
            Dim cross3 = New Vector3(p2.Y * p1.Z - p2.Z * p1.Y, p2.Z * p1.X - p2.X * p1.Z, p2.X * p1.Y - p2.Y * p1.X) 'Vector3.Cross(p2, p1)

            Dim check1 = (cross1.X * cross2.X + cross1.Y * cross2.Y + cross1.Z * cross2.Z) > 0 'Vector3.Dot(cross1, cross2) > 0
            Dim check2 = (cross3.X * -cross1.X + cross3.Y * -cross1.Y + cross3.Z * -cross1.Z) > 0 'Vector3.Dot(cross3, -cross1) > 0
            Dim check3 = (-cross3.X * -cross2.X + -cross3.Y * -cross2.Y + -cross3.Z * -cross2.Z) > 0 'Vector3.Dot(-cross3, -cross2) > 0

            'don't need to calculate all checks at the start, neither cross3 if finish early
            If check1 OrElse check2 OrElse check3 Then
                If currentNode.self.triangles.Count > 1 Then 'check other triangles on the same plane
                    For x = 1 To currentNode.self.triangles.Count - 1
                        p1 = vBuffer(iBuffer(currentNode.self.triangles(x)).X - 1) - intersect
                        p2 = vBuffer(iBuffer(currentNode.self.triangles(x) + 1).X - 1) - intersect
                        p3 = vBuffer(iBuffer(currentNode.self.triangles(x) + 2).X - 1) - intersect

                        cross1 = New Vector3(p3.Y * p2.Z - p3.Z * p2.Y, p3.Z * p2.X - p3.X * p2.Z, p3.X * p2.Y - p3.Y * p2.X) 'Vector3.Cross(p3, p2)
                        cross2 = New Vector3(p3.Y * p1.Z - p3.Z * p1.Y, p3.Z * p1.X - p3.X * p1.Z, p3.X * p1.Y - p3.Y * p1.X) 'Vector3.Cross(p3, p1)
                        cross3 = New Vector3(p2.Y * p1.Z - p2.Z * p1.Y, p2.Z * p1.X - p2.X * p1.Z, p2.X * p1.Y - p2.Y * p1.X) 'Vector3.Cross(p2, p1)

                        check1 = (cross1.X * cross2.X + cross1.Y * cross2.Y + cross1.Z * cross2.Z) > 0 'Vector3.Dot(cross1, cross2)
                        check2 = (cross3.X * -cross1.X + cross3.Y * -cross1.Y + cross3.Z * -cross1.Z) > 0 'Vector3.Dot(cross3, -cross1)
                        check3 = (-cross3.X * -cross2.X + -cross3.Y * -cross2.Y + -cross3.Z * -cross2.Z) > 0 'Vector3.Dot(-cross3, -cross2)

                        If Not (check1 OrElse check2 OrElse check3) Then
                            Return True 'passed all intersection checks
                        End If
                    Next
                End If

                Return False
            End If

            Return True 'passed all intersection tests
        End Function

        Public Function Iterate(dir As Integer, cam As Camera) As List(Of Node)
            Dim result As New List(Of Node)
            Iteration(start, dir, cam, result)
            Return result
        End Function

        'adds items to a list as the tree is traversed (traversal direction based on dir: 1 = back-to-front; -1 = front-to-back)
        <MethodImpl(MethodImplOptions.AggressiveOptimization)>
        Private Sub Iteration(current As Node, dir As Integer, cam As Camera, ByRef list As List(Of Node))
            'if leaf node, then add self to list and exit early for efficiency
            If current.front.Count = 0 AndAlso current.behind.Count = 0 Then
                list.Add(current)
                Exit Sub
            End If

            'calculate cam pos relative to plane
            Dim p = Vector3.Dot(current.self.normal, cam.pos) - current.self.planeConstant 'where -ve is behind plane, +ve in front of plane, 0 on plane
            p *= dir 'ensure correct traversal direction

            'determine which child to iterate to next (if dir = 1, then this will result in back-to-front traversal)
            If p >= 0 Then 'in front of plane
                'behind first
                If current.behind.Count > 1 Then
                    Quicksort(current.behind, 0, current.behind.Count, cam, dir) 'sort grouped leaves
                    For n = 0 To current.behind.Count - 1
                        list.Add(current.behind(n))
                    Next
                ElseIf current.behind.Count = 1 Then
                    Iteration(current.behind(0), dir, cam, list)
                End If

                list.Add(current)

                    If current.front.Count > 1 Then
                    Quicksort(current.front, 0, current.front.Count, cam, dir) 'sort grouped leaves
                    For n = 0 To current.front.Count - 1
                        list.Add(current.front(n))
                    Next
                ElseIf current.front.Count = 1 Then
                    Iteration(current.front(0), dir, cam, list)
                End If

            Else 'behind plane
                'front first
                If current.front.Count > 1 Then
                    Quicksort(current.front, 0, current.front.Count, cam, dir) 'sort grouped leaves
                    For n = 0 To current.front.Count - 1
                        list.Add(current.front(n))
                    Next
                ElseIf current.front.Count = 1 Then
                    Iteration(current.front(0), dir, cam, list)
                End If

                list.Add(current)

                    If current.behind.Count > 1 Then
                    Quicksort(current.behind, 0, current.behind.Count, cam, dir) 'sort grouped leaves
                    For n = 0 To current.behind.Count - 1
                        list.Add(current.behind(n))
                    Next
                ElseIf current.behind.Count = 1 Then
                    Iteration(current.behind(0), dir, cam, list)
                End If
            End If
        End Sub

        'where dir determines whether the quicksort should sort lowest-to-highest or highest-to-lowest depth
        Sub Quicksort(ByRef sort As List(Of Node), ByVal left As Integer, ByVal right As Integer, cam As Camera, dir As Integer)
            'determine whether or not to switch to insertion sort
            Dim length As Integer = right - left

            Dim pivot As Integer
            If length > 1 Then
                pivot = (left + right) / 2

                'partition (ie. sort the pivot so it is ordered)
                Dim i As Integer, piv As Node, store As Integer

                piv = sort(pivot)

                Dim tmp As Node
                tmp = sort(right - 1)
                sort(right - 1) = sort(pivot)
                sort(pivot) = tmp
                store = left

                'new depth calculation code
                Dim p1 = (vBuffer(iBuffer(piv.self.triangles(0)).X - 1) - cam.pos).Length
                Dim p2 = (vBuffer(iBuffer(piv.self.triangles(0) + 1).X - 1) - cam.pos).Length
                Dim p3 = (vBuffer(iBuffer(piv.self.triangles(0) + 2).X - 1) - cam.pos).Length
                Dim point1 As Single

                If dir = -1 Then 'want furthest (for front-to-back)
                    If p1 > p2 AndAlso p1 > p3 Then
                        point1 = p1
                    ElseIf p2 > p1 AndAlso p2 > p3 Then
                        point1 = p2
                    Else
                        point1 = p3
                    End If

                Else
                    If p1 < p2 AndAlso p1 < p3 Then
                        point1 = p1
                    ElseIf p2 < p1 AndAlso p2 < p3 Then
                        point1 = p2
                    Else
                        point1 = p3
                    End If

                End If

                For i = left To right - 2

                    Dim point2 As Single

                    'calculating closest depth (if dir = 1 => back to front)
                    p1 = (vBuffer(iBuffer(sort(i).self.triangles(0)).X - 1) - cam.pos).Length
                    p2 = (vBuffer(iBuffer(sort(i).self.triangles(0) + 1).X - 1) - cam.pos).Length
                    p3 = (vBuffer(iBuffer(sort(i).self.triangles(0) + 2).X - 1) - cam.pos).Length

                    If dir = -1 Then
                        If p1 > p2 AndAlso p1 > p3 Then
                            point2 = p1
                        ElseIf p2 > p1 AndAlso p2 > p3 Then
                            point2 = p2
                        Else
                            point2 = p3
                        End If

                    Else 'closest (for back-to-front)
                        If p1 < p2 AndAlso p1 < p3 Then
                            point2 = p1
                        ElseIf p2 < p1 AndAlso p2 < p3 Then
                            point2 = p2
                        Else
                            point2 = p3
                        End If

                    End If

                    If point2 * -dir <= point1 * -dir Then 'compare distance to camera
                        'switch here (sorting the pivot)
                        tmp = sort(store)
                        sort(store) = sort(i)
                        sort(i) = tmp

                        store += 1
                    End If
                Next

                tmp = sort(store)
                sort(store) = sort(right - 1)
                sort(right - 1) = tmp

                'set pivot
                pivot = store

                'recursively sort sub-arrays (all the values to the left will be under current pivot, all values to right will be over current pivot // depending on direction given)
                Quicksort(sort, left, pivot, cam, dir)
                Quicksort(sort, pivot + 1, right, cam, dir)
            End If
        End Sub
    End Class
End Class

Structure Quaternion
    Public Q As Vector4 'where w represents the real part

    Public Sub New(X As Single, Y As Single, Z As Single, W As Single)
        Q = New Vector4(X, Y, Z, W)
    End Sub

    Shared Function MatrixToQuaternion(m As Pipeline.Transform) As Quaternion
        Dim t As Single
        Dim q As Quaternion
        If m.z.Z < 0 Then 'see referenced material for mathematical derivation
            If m.x.X > m.y.Y Then
                t = 1 + m.x.X - m.y.Y - m.z.Z
                q = New Quaternion(t, m.x.Y + m.y.X, m.z.X + m.x.Z, m.y.Z - m.z.Y)
            Else
                t = 1 - m.x.X + m.y.Y - m.z.Z
                q = New Quaternion(m.x.Y + m.y.X, t, m.y.Z + m.z.Y, m.z.X - m.x.Z)
            End If
        Else
            If m.x.X < -m.y.Y Then
                t = 1 - m.x.X - m.y.Y + m.z.Z
                q = New Quaternion(m.z.X + m.x.Z, m.y.Z + m.z.Y, t, m.x.Y - m.y.X)
            Else
                t = 1 + m.x.X + m.y.Y + m.z.Z
                q = New Quaternion(m.y.Z - m.z.Y, m.z.X - m.x.Z, m.x.Y - m.y.X, t)
            End If
        End If
        q.Q *= 0.5 / Math.Sqrt(t)
        Return q
    End Function

    Shared Function QuaternionToMatrix(q As Quaternion) As Pipeline.Transform
        Dim tr As New Pipeline.Transform With {
            .x = New Vector4,
            .y = New Vector4,
            .z = New Vector4,
            .w = New Vector4(New Vector3, 1)}
        Dim Quat = q.Q

        'this is using the referenced material, but in a reversed process
        tr.x.X = 1 - (2 * (Quat.Y ^ 2)) - (2 * (Quat.Z ^ 2))
        tr.x.Y = 2 * (Quat.X * Quat.Y + Quat.Z * Quat.W)
        tr.x.Z = 2 * (Quat.X * Quat.Z - Quat.Y * Quat.W)

        tr.y.X = 2 * (Quat.X * Quat.Y - Quat.Z * Quat.W)
        tr.y.Y = 1 - (2 * (Quat.X ^ 2)) - (2 * (Quat.Z ^ 2))
        tr.y.Z = 2 * (Quat.Y * Quat.Z + Quat.X * Quat.W)

        tr.z.X = 2 * (Quat.X * Quat.Z + Quat.Y * Quat.W)
        tr.z.Y = 2 * (Quat.Y * Quat.Z - Quat.X * Quat.W)
        tr.z.Z = 1 - (2 * (Quat.X ^ 2)) - (2 * (Quat.Y ^ 2))

        Return tr
    End Function

    Shared Function CreateFromAxis(x As Single, y As Single, z As Single, w As Single) As Quaternion
        Dim factor = Math.Sin(w / 2)
        Dim q As New Quaternion(x * factor, y * factor, z * factor, Math.Cos(w / 2))
        q.Q = Vector4.Normalize(q.Q)
        Return q
    End Function

    Shared Operator *(q1 As Quaternion, q2 As Quaternion) As Quaternion
        Dim sa = q1.Q.W
        Dim sb = q2.Q.W
        Dim va = New Vector3(q1.Q.X, q1.Q.Y, q1.Q.Z)
        Dim vb = New Vector3(q2.Q.X, q2.Q.Y, q2.Q.Z)
        Dim scalar = sa * sb - Vector3.Dot(va, vb) 'result of multiplying out the real terms
        Dim vector = Vector3.Cross(va, vb) + sa * vb + sb * va 'result of multiplying out all imaginary terms
        Return New Quaternion(vector.X, vector.Y, vector.Z, scalar)
    End Operator

    Shared Function Inverse(q As Quaternion) As Quaternion 'negate vector parts
        Return New Quaternion With {.Q = New Vector4(-q.Q.X, -q.Q.Y, -q.Q.Z, q.Q.W)}
    End Function
End Structure

Class FileParser
    Public Structure Buffers
        Dim vertex() As Vector3
        Dim index() As Base.Tuple3
        Dim tex As Tex
    End Structure
    Public Structure Tex
        Dim texCoord() As Vector2
        Dim texVal()() As Integer
        Dim texIndices() As Vector2
        Dim texSize() As Vector2
        Dim transparency() As Single
    End Structure

    Public Shared supported() As String = {".obj", ".scn"}


    'LoadFile should just determine the file type, and there should be a couple of overloads for different things it needs to return

    'will need to pull most of this code out, and have LoadFile start by determining the file type (and also handling for file types not supported yet)
    'This will automatically try to load and fill as many of the reference arrays with data, if possible (ie. given correct texture data for example)
    Public Shared Function LoadSCN(de As DevelopmentEnvironment, filePath As String, format As PixelFormat, sc As Scene, ByRef outModels()() As BVH.AABB(Of Model), ByRef outBvs() As BVH.AABB(Of BVH.BV), ByRef outLights() As PointLight, ByRef outAmbient As Integer, ByRef root As BVH.Tuple2) As Boolean
        Dim fileContents() As Byte
        Dim ext = Split(filePath, ".")
        Dim e = ext(ext.Length - 1)
        If e <> "scn" Then
            MessageBox.Show("Error: Invalid File Type. File may be corrupt.")
            Return False
        End If

        Dim s = IO.File.Open(filePath, FileMode.OpenOrCreate, FileAccess.Read)
        Dim file As New BinaryReader(s)

        fileContents = file.ReadBytes(s.Length) 'don't need to stream the file since it is so small
        file.Close()
        s.Close()

        Dim ptr As Integer = 0
        Dim valid As Boolean
        Dim amount = ReadTag(fileContents, ptr, "<Bases>", valid, 0) 'read first tag (for bases)
        If Not valid Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If

        Dim bases(amount - 1) As Base
        Dim cont As Boolean = False
        sc.newID = 0

        Dim out(amount - 1) As Buffers
        Dim max(amount - 1) As Vector3
        Dim min(amount - 1) As Vector3
        Dim upper(amount - 1) As Vector3
        Dim lower(amount - 1) As Vector3
        Dim result(amount - 1) As Integer
        Dim path(amount - 1) As String

        For i = 0 To amount - 1 'parallel model loading
            result(i) = 2 'default flag; finished when either 0 or 1 (depending on outcome)
            path(i) = ReadString(fileContents, ptr)
            Dim index = i
            Dim loadThread As New Thread(Sub()
                                             result(index) = LoadFile(de, path(index), out(index), format, max(index), min(index))
                                         End Sub)
            loadThread.Start() 'run in parallel
            upper(i) = ReadV3(fileContents, ptr)
            lower(i) = ReadV3(fileContents, ptr)
        Next

        'wait for all models to load
        While Not cont
            cont = True
            For i = 0 To result.Length - 1
                If result(i) <> 2 Then
                    If result(i) = False Then 'one of the bases didn't load correctly
                        MessageBox.Show("Error: Invalid Model Data. File may be corrupt.")
                        Return False
                    End If
                Else
                    cont = False
                End If
            Next
        End While

        For i = 0 To amount - 1
            bases(i) = New Base(path(i), sc.newID, out(i).vertex, out(i).index, out(i).tex.texCoord, out(i).tex.texVal, out(i).tex.texSize, out(i).tex.transparency, max(i), min(i)) With {
            .upper = upper(i),
            .lower = lower(i)}
            sc.newID += 1
        Next

        Dim models(amount - 1)() As BVH.AABB(Of Model)
        Dim total = ReadTag(fileContents, ptr, "<Models>", valid, 0) 'read second tag (for individual models)
        If Not valid Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If
        Dim mCount = ReadInt(fileContents, ptr) 'amount holds the amount of models for the first base
        Dim runningTotal As Integer = mCount
        If runningTotal > total Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If

        For i = 0 To amount - 1
            Dim temp(mCount - 1) As BVH.AABB(Of Model)
            For m = 0 To mCount - 1 'read in individual model data
                Try
                    temp(m) = New BVH.AABB(Of Model) With {.parent = ReadInt(fileContents, ptr), 'read in AABB data
                        .upper = ReadV3(fileContents, ptr),
                        .lower = ReadV3(fileContents, ptr),
                        .contents = New Model With {.contents = bases(i), 'read in model data
                            .translation = ReadV3(fileContents, ptr),
                            .rot = ReadV3(fileContents, ptr),
                            .scale = ReadV3(fileContents, ptr),
                            .depth0 = ReadBoolean(fileContents, ptr),
                            .lightRef = ReadInt(fileContents, ptr)}}
                Catch
                    MessageBox.Show("Error: Insufficient Scene Data. File may be corrupt.")
                    Return False
                End Try
            Next

            models(i) = temp 'organise the jagged array correctly (so each array represents a unique base)
            If i <> amount - 1 Then 'get new model count for next base (if not at end already)
                mCount = ReadInt(fileContents, ptr)
                If mCount <= 0 OrElse runningTotal + mCount > total Then
                    MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
                    Return False
                End If
                runningTotal += mCount
            End If
        Next

        'ensure total is matched
        If runningTotal > total Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If

        amount = ReadTag(fileContents, ptr, "<BVs>", valid, 0) 'third tag (BV)
        If Not valid Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If
        Dim bvs(amount - 1) As BVH.AABB(Of BVH.BV)
        root = New BVH.Tuple2 With {.X = ReadInt(fileContents, ptr), .Y = ReadInt(fileContents, ptr)}

        For i = 0 To amount - 1
            Try
                bvs(i) = New BVH.AABB(Of BVH.BV) With {.parent = ReadInt(fileContents, ptr), 'read in AABB data
                .upper = ReadV3(fileContents, ptr),
                .lower = ReadV3(fileContents, ptr),
                .contents = New BVH.BV With { 'read in BV data
                    .child1 = New BVH.Tuple2 With {.X = ReadInt(fileContents, ptr), .Y = ReadInt(fileContents, ptr)},
                    .child2 = New BVH.Tuple2 With {.X = ReadInt(fileContents, ptr), .Y = ReadInt(fileContents, ptr)}}}
            Catch
                MessageBox.Show("Error: Insufficient Scene Data. File may be corrupt.")
                Return False
            End Try
        Next

        amount = ReadTag(fileContents, ptr, "<Lighting>", valid, -1) 'fourth tag (lighting)
        If Not valid Then
            MessageBox.Show("Error: Invalid Scene Data. File may be corrupt.")
            Return False
        End If
        Dim lights(amount - 1) As PointLight

        If amount > 0 Then
            For i = 0 To amount - 1
                Try
                    lights(i) = New PointLight With {.pos = ReadV3(fileContents, ptr), 'get light data
                        .diffuse = ReadInt(fileContents, ptr), .Range = ReadSingle(fileContents, ptr),
                        .constModifier = ReadSingle(fileContents, ptr), .linModifier = ReadSingle(fileContents, ptr), .expModifier = ReadSingle(fileContents, ptr)}
                Catch
                    MessageBox.Show("Error: Insufficient Scene Data. File may be corrupt.")
                    Return False
                End Try
            Next
        End If
        Dim ambient = ReadInt(fileContents, ptr) 'get ambient value (last thing in the file)

        If ambient <> -1 AndAlso ptr = fileContents.Length Then 'correctly read entire file
            outModels = models 'only overwrite / load in scene information once it is verified to be correct
            outBvs = bvs
            outLights = lights
            outAmbient = ambient
        End If
        Return True
    End Function

    Private Shared Function ReadV3(array() As Byte, ByRef start As Integer) As Vector3
        Return New Vector3(ReadSingle(array, start), ReadSingle(array, start), ReadSingle(array, start))
    End Function

    Private Shared Function ReadSingle(array() As Byte, ByRef start As Integer) As Single
        If start > array.Length - 1 Then start = -1 : Return -1.0F
        Dim s = BitConverter.ToSingle(array, start)
        start += 4
        Return s
    End Function

    Private Shared Function ReadTag(array() As Byte, ByRef start As Integer, expected As String, ByRef valid As Boolean, limit As Integer) As Integer
        Dim tag As String = ReadString(array, start)
        Dim amount As Integer
        valid = True

        If tag <> expected Then
            valid = False
        Else
            amount = ReadInt(array, start)
            If amount <= limit Then
                valid = False
            End If
        End If

        Return amount
    End Function

    Private Shared Function ReadBoolean(array() As Byte, ByRef start As Integer) As Boolean
        If start > array.Length - 1 Then start = -1 : Return -1
        Dim i = BitConverter.ToBoolean(array, start)
        start += 1
        Return i
    End Function
    Private Shared Function ReadInt(array() As Byte, ByRef start As Integer) As Integer
        If start > array.Length - 1 Then start = -1 : Return -1
        Dim i = BitConverter.ToInt32(array, start)
        start += 4
        Return i
    End Function

    'where the first value is the length of the string
    Private Shared Function ReadString(array() As Byte, ByRef start As Integer) As String
        If start > array.Length - 1 Then start = -1 : Return ""
        Dim l = array(start)
        Dim ch(l - 1) As Char
        If array.Length - start > l Then
            For i = 1 To l
                start += 1
                ch(i - 1) = ChrW(array(start))
            Next
        End If
        start += 1
        Return New String(ch)
    End Function

    Public Shared Function LoadFile(de As DevelopmentEnvironment, filePath As String, ByRef inputBuffers As Buffers, format As PixelFormat, ByRef max As Vector3, ByRef min As Vector3) As Boolean
        'Start by loading the file into an array for quicker access
        Dim fileContents() As String
        Dim ext = Split(filePath, ".")
        Dim e = ext(ext.Length - 1)
        If e <> "obj" Then
            MessageBox.Show("Error: Invalid File Type. File may be corrupt.")
            Return False
        End If

        Dim openFile As StreamReader
        Dim part = Split(filePath, "\", 2)
        Dim fileSize As Single
        Try
            openFile = New StreamReader(filePath)
            fileSize = FileLen(filePath)
        Catch
            Try 'attempt to open the file on: the default C drive, this current drive this app is on (if not C)
                openFile = New StreamReader("C:\" & part(1))
                fileSize = FileLen("C:\" & part(1))
                filePath = "C:\" & part(1)
            Catch
                Try
                    If Environment.CurrentDirectory(0) <> "C" Then
                        openFile = New StreamReader(Environment.CurrentDirectory(0) & ":\" & part(1))
                        fileSize = FileLen(Environment.CurrentDirectory(0) & ":\" & part(1))
                        filePath = Environment.CurrentDirectory(0) & ":\" & part(1) 'save the correct file path
                    Else
                        Error "Invalid Path"
                    End If
                Catch
                    MessageBox.Show("Error: Cannot find one or more files. File may be corrupt.")
                    Return False
                End Try
            End Try
        End Try

        Dim path As String = IO.Path.GetDirectoryName(filePath) & "\" 'input this as path for mtl file

        de.Dispatcher.Invoke(Sub()
                                 de.LoadingBar.Visibility = Visibility.Visible
                                 de.LoadBar.Maximum += fileSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)


        'Read in data and format
        Dim contents As New List(Of String)
        Dim current As String

        While Not openFile.EndOfStream
            current = openFile.ReadLine
            If current.Length > 0 Then
                contents.Add(current)
            End If
        End While
        fileContents = contents.ToArray()

        contents = Nothing

        openFile.Close()
        openFile.Dispose()

        'face indices correspond directly to the vertex lists as if they were joined, so the vertex lists can be saved in to the buffer consecutively
        'Just need to find the start of each vertex list, use spans to get each list in memory, and then convert each to an array and then join thems (concat straight to output buffers)
        'Can do the same thing with the face and texcoord lists at the same time (although any faces with 4 points will require subdividing first, but this can be done afterwards)
        'Also need to record (after which face list) new textures start

        'The length of the lists is equal to (vEnd - vStart) + 1
        Dim vStart As Integer = -1 'start as 0 indexed

        Dim tStart As Integer = -1

        'face indices are 1 based, so before use, they must be subtracted by one (can be done after, in parallel)
        Dim fStart As Integer = -1
        Dim prevfEnd As Integer = -1

        Dim newTBegin As New List(Of Vector2) 'as an index to face / index buffer, where from this face index (inclusive), the second referencing which texture in the jagged array to use
        Dim texList(-1) As Tuple(Of String, Integer) 'where the first holds the material name, and the second holds the index of jagged array where the texture is stored (newTBegin use this to reference texture)

        Dim fullVBuffer() As String = {} 'used to store the full vertex lists, before converting them to vector 3s
        Dim fullTBuffer() As String = {} 'both of these buffers will need to have the single values extracted from them
        Dim fullIBuffer() As String = {}

        Dim usemtlF As Boolean = False
        Dim mtllibF As Boolean = False

        Dim progress As Single = fileSize / fileContents.Length
        Dim currentProgress As Single = 0

        For i = 0 To fileContents.Length - 1
            If fileContents(i)(0) = "v"c AndAlso fileContents(i)(1) = " "c Then 'vertex list
                If vStart = -1 Then
                    vStart = i
                End If

            ElseIf i > 0 Then 'check previous line
                If fileContents(i - 1)(0) = "v"c AndAlso fileContents(i - 1)(1) = " "c Then
                    Dim listSpan = fileContents.AsMemory.Slice(vStart, ((i - 1) - vStart) + 1) 'can replace with asSpan
                    vStart = -1
                    fullVBuffer = fullVBuffer.Concat(listSpan.ToArray()).ToArray()
                End If

            End If

            If fileContents(i)(0) = "v"c AndAlso fileContents(i)(1) = "t"c Then 'texture coordinates
                If tStart = -1 Then
                    tStart = i
                End If

            ElseIf i > 0 Then 'check previous line
                If fileContents(i - 1)(0) = "v"c AndAlso fileContents(i - 1)(1) = "t"c Then
                    Dim listSpan = fileContents.AsMemory.Slice(tStart, ((i - 1) - tStart) + 1)
                    tStart = -1
                    fullTBuffer = fullTBuffer.Concat(listSpan.ToArray()).ToArray()
                End If

            End If

            If fileContents(i)(0) = "f" Then
                If fStart = -1 Then
                    fStart = i
                End If

            ElseIf i > 0 Then 'check previous line
                If fileContents(i - 1)(0) = "f" Then
                    Dim listSpan = fileContents.AsMemory.Slice(fStart, ((i - 1) - fStart) + 1)
                    fStart = -1
                    fullIBuffer = fullIBuffer.Concat(listSpan.ToArray()).ToArray()
                    prevfEnd = fullIBuffer.Length - 1 'index of last face here
                End If

            End If

            'special check (last line) for faces
            If i = fileContents.Length - 1 AndAlso fileContents(i)(0) = "f" Then
                Dim listSpan = fileContents.AsMemory.Slice(fStart, ((i) - fStart) + 1)
                fStart = -1
                fullIBuffer = fullIBuffer.Concat(listSpan.ToArray()).ToArray()
                prevfEnd = fullIBuffer.Length - 1
            End If

            'checks for textures ============================================
            If Left(fileContents(i), 6) = "mtllib" Then
                Dim newFile() As String = Split(fileContents(i), " ")
                Try
                    OpenMTL(de, path, newFile(1), texList, inputBuffers.tex.texVal, format, inputBuffers.tex.texSize, inputBuffers.tex.transparency)
                    If texList.Count = 0 OrElse inputBuffers.tex.texVal.Length = 0 Then
                        MessageBox.Show("Error: Invalid Texture File. File may be corrupt.")
                        'Return False
                    End If
                Catch
                    MessageBox.Show("Error: Invalid Texture File. File may be corrupt.")
                    Return False
                End Try
                mtllibF = True

            ElseIf Left(fileContents(i), 6) = "usemtl" Then
                'use prevfEnd + 1 as index to new face array that will use this material, or if prevfEnd = -1, then use 0
                Dim newMat() As String = Split(fileContents(i), " ")
                Dim indexF As Integer
                Dim indexT As Integer
                usemtlF = True

                If prevfEnd = -1 Then
                    indexF = 0
                Else
                    indexF = prevfEnd + 1
                End If

                For j = 0 To texList.Length - 1
                    If newMat(1) = texList(j).Item1 Then
                        indexT = j
                        Exit For
                    End If
                Next
                newTBegin.Add(New Vector2(indexF, indexT))
            End If

            'too expensive to run every line; aim to update once for each whole point gotten (when accrued progress >=1)
            currentProgress += progress
            If currentProgress >= (fileSize / 100) OrElse i = fileContents.Length - 1 Then
                de.Dispatcher.Invoke(Sub()
                                         de.LoadBar.Value += currentProgress
                                         de.LoadBar.InvalidateVisual()
                                     End Sub)
                currentProgress = 0
            End If
        Next
        fileContents = Nothing

        If mtllibF = False Then
            MessageBox.Show("Error: No Texture File Found. File may be corrupt.")
            Return False
        ElseIf usemtlF = False Then
            MessageBox.Show("Error: No Textures Used. File may be corrupt.")
            Return False
        End If

        inputBuffers.tex.texIndices = newTBegin.ToArray()

        'convert the full v buffer into vector3 format, as well as the other two into single and integer formats
        Dim vList As New List(Of Vector3)
        Dim tList As New List(Of Vector2)
        Dim iList As New List(Of Base.Tuple3)

        Dim totalSize = 0
        Dim size = 0
        Try
            For i = 0 To fullVBuffer.Length - 1
                size += fullVBuffer(i).Length * 4
            Next
            totalSize += size
            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Maximum += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)

            For i = 0 To fullVBuffer.Length - 1
                Dim line() As String = Split(fullVBuffer(i), " ", 4) 'expecting 4 values: "v", x, y, z
                vList.Add(New Vector3(line(1), line(2), line(3)))
            Next

            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Value += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)

            size = 0
            For i = 0 To fullTBuffer.Length - 1
                size += fullTBuffer(i).Length * 4
            Next
            totalSize += size
            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Maximum += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)

            For i = 0 To fullTBuffer.Length - 1
                Dim line() As String = Split(fullTBuffer(i), " ", 3) 'expecting 3 values: "vt", u, v
                tList.Add(New Vector2(line(1), line(2)))
            Next

            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Value += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)

            Dim count As Integer = 0 'used to check the textureIndices and set it properly
            Dim v4Count As Integer = 0
            Dim a As Integer = 1

            size = 0
            For i = 0 To fullIBuffer.Length - 1
                size += fullIBuffer(i).Length * 4
            Next
            totalSize += size
            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Maximum += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)
            For i = 0 To fullIBuffer.Length - 1
                Dim line() As String = Split(fullIBuffer(i), " ") 'expecting values: "f", face1, face2, face3, face4...
                Dim sectLength As Integer = line.Length - 2 '-2 to account for first value "f"
                Dim sections(sectLength)() As String

                'need to clean (may be some 's' lines here that can be ignored)
                If line(0) = "s"c Then
                    Continue For
                End If

                For j = 0 To sectLength
                    sections(j) = Split(line(j + 1), "/", 3) 'expecting 3 values: v,vt,vn (will only use v and vt)
                Next

                'triangulate (always start with origin / first point, then the previous point, then the current point) (apparently this is called fan triangulation)
                'this works with any amount of points providing: ccw winding, no holes, concave geometry
                Dim orig = 0
                Dim prev As Integer

                For j = 2 To sectLength
                    If sections(j)(0) = "" OrElse sections(j)(1) = "" Then
                        Exit For
                    End If
                    prev = j - 1
                    iList.Add(New Base.Tuple3 With {.X = sections(orig)(0), .Y = sections(orig)(1), .Z = 0})
                    iList.Add(New Base.Tuple3 With {.X = sections(prev)(0), .Y = sections(prev)(1), .Z = 0})
                    iList.Add(New Base.Tuple3 With {.X = sections(j)(0), .Y = sections(j)(1), .Z = 0})
                Next

                'update texture indices to match the index buffer positions (so that the correct texture index can be found later)
                count += 1
                If a < inputBuffers.tex.texIndices.Length Then
                    If count >= inputBuffers.tex.texIndices(a).X Then
                        inputBuffers.tex.texIndices(a).X *= 3
                        inputBuffers.tex.texIndices(a).X += v4Count
                        a += 1
                        v4Count = 0
                    End If
                End If
            Next
            de.Dispatcher.Invoke(Sub()
                                     de.LoadBar.Value += size
                                     de.LoadBar.InvalidateVisual()
                                 End Sub)
        Catch
            MessageBox.Show("Error: Invalid Syntax. File may be corrupt.")
            Return False
        End Try

        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Maximum += totalSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)

        inputBuffers.vertex = vList.ToArray()
        inputBuffers.tex.texCoord = tList.ToArray()
        inputBuffers.index = iList.ToArray()

        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Value += totalSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)

        If iList.Count = 0 Then
            MessageBox.Show("Error: No Triangles. File may be corrupt.")
            Return False
        End If

        vList = Nothing
        tList = Nothing
        iList = Nothing

        'loop through the index buffer and ensure that the indices are valid
        For n = 0 To inputBuffers.index.Length - 1
            Dim i1 = inputBuffers.index(n).X - 1
            Dim i2 = inputBuffers.index(n).Y - 1
            If i1 > inputBuffers.vertex.Length OrElse i1 < 0 OrElse i2 > inputBuffers.tex.texCoord.Length - 1 OrElse i2 < 0 Then
                MessageBox.Show("Error: Invalid References. File may be corrupt.")
                Return False
            End If
        Next

        'loop through the vertex coordinate buffer and ensure the indices are valid
        For n = 0 To inputBuffers.tex.texCoord.Length - 1
            Dim t = inputBuffers.tex.texCoord(n)
            If t.X < 0 OrElse t.X > 1 OrElse t.Y < 0 OrElse t.Y > 1 Then
                MessageBox.Show("Error: Invalid Texture Coordinates. File may be corrupt.")
                Return False
            End If
        Next

        'Loop through tbuffer, find the correct texture index according to the textureindices array
        'Also multiply out the texture coords according to the texture size that they are for
        Dim correctTexture As Integer = inputBuffers.tex.texIndices(0).Y

        size = 0
        For i = 0 To inputBuffers.index.Length - 1
            size += 12 * 4 '(3 integers => 12 * 4)
        Next
        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Maximum += size
                                 de.LoadBar.InvalidateVisual()
                             End Sub)
        For i = 0 To inputBuffers.index.Length - 1
            For j = 0 To inputBuffers.tex.texIndices.Length - 1
                If i >= inputBuffers.tex.texIndices(j).X Then
                    If j <> inputBuffers.tex.texIndices.Length - 1 Then
                        If i < inputBuffers.tex.texIndices(j + 1).X Then 'between the two
                            correctTexture = inputBuffers.tex.texIndices(j).Y
                            Exit For
                        End If
                    Else 'is the last
                        correctTexture = inputBuffers.tex.texIndices(j).Y
                    End If
                End If
            Next
            inputBuffers.index(i).Z = correctTexture
        Next
        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Value += size
                                 de.LoadBar.InvalidateVisual()
                             End Sub)

        'determine the scale of the model (find the bounding box); centre the model according to the bounding box and then scale smaller
        min = inputBuffers.vertex(0)
        max = inputBuffers.vertex(0)

        For i = 1 To inputBuffers.vertex.Length - 1
            Dim toCheck = inputBuffers.vertex(i)

            'check against min
            If min.X > toCheck.X Then
                min.X = toCheck.X
            End If
            If min.Y > toCheck.Y Then
                min.Y = toCheck.Y
            End If
            If min.Z > toCheck.Z Then
                min.Z = toCheck.Z
            End If

            'check against max
            If max.X < toCheck.X Then
                max.X = toCheck.X
            End If
            If max.Y < toCheck.Y Then
                max.Y = toCheck.Y
            End If
            If max.Z < toCheck.Z Then
                max.Z = toCheck.Z
            End If
        Next

        Dim centre As New Vector3((max.X + min.X) / 2, (max.Y + min.Y) / 2, (max.Z + min.Z) / 2) 'need to translate by -centre
        Dim diff = max - min
        Dim rInput = inputBuffers

        'translate
        Parallel.For(0, inputBuffers.vertex.Length,
            Sub(i)
                rInput.vertex(i) = rInput.vertex(i) - centre
            End Sub)

        'setup for scaling
        Dim chosen As Single
        If diff.X > diff.Y AndAlso diff.X > diff.Z Then
            chosen = diff.X
        ElseIf diff.Y > diff.Z Then
            chosen = diff.Y
        Else
            chosen = diff.Z
        End If

        Dim factor As Single = 1 / (chosen / 2) 'scale so that the largest dimension becomes scale of 1 (keeps the model proportional still)
        Dim scale As New Vector3(factor, factor, factor)

        'scale
        Dim nMax As New Vector3(Single.MinValue)
        Dim nMin As New Vector3(Single.MaxValue)
        Parallel.For(0, inputBuffers.vertex.Length,
            Sub(i)
                rInput.vertex(i) = rInput.vertex(i) * scale
            End Sub)

        For i = 0 To inputBuffers.vertex.Length - 1
            'correct the boundaries
            'check against min
            Dim toCheck = rInput.vertex(i)
            If nMin.X > toCheck.X Then
                nMin.X = toCheck.X
            End If
            If nMin.Y > toCheck.Y Then
                nMin.Y = toCheck.Y
            End If
            If nMin.Z > toCheck.Z Then
                nMin.Z = toCheck.Z
            End If

            'check against max
            If nMax.X < toCheck.X Then
                nMax.X = toCheck.X
            End If
            If nMax.Y < toCheck.Y Then
                nMax.Y = toCheck.Y
            End If
            If nMax.Z < toCheck.Z Then
                nMax.Z = toCheck.Z
            End If
        Next

        min = nMin
        max = nMax

        Return True 'no errors
    End Function

    Public Shared Sub OpenMTL(de As DevelopmentEnvironment, filePath As String, fileName As String, ByRef matContents() As Tuple(Of String, Integer), ByRef texture()() As Integer, format As PixelFormat, ByRef texSize() As Vector2, ByRef transparency() As Single)
        'open mtl file, add texture details to matContents in the order the textures are set in the file
        'Then load the textures into texture byte array in said order

        'Start by loading the file into an array for quicker access
        Dim fileContents() As String
        Dim openFile As New StreamReader(filePath & fileName)

        Dim contents As New List(Of String)
        Dim current As String

        While Not openFile.EndOfStream
            current = openFile.ReadLine
            If current.Length > 0 Then
                contents.Add(current)
            End If

        End While

        fileContents = contents.ToArray()
        openFile.Close()
        openFile.Dispose()

        'Load from file into bitmap, then use bitmap.CopyPixels to transfer into array
        'Can redimension texture()() first according to the length of texList
        Dim matList As New List(Of Tuple(Of String, Integer)) 'add details here before setting to matContents
        Dim tr As New List(Of Single)

        Dim tList As New List(Of Integer()) 'testing
        Dim sList As New List(Of Vector2)
        Dim mCount As Integer = 0
        Dim tCount As Integer = 0
        Dim tNotFound As Boolean = False

        Dim fileSize As Integer = FileLen(filePath & fileName)
        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Maximum += fileSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)
        Dim progress = fileSize / fileContents.Length
        Dim currentProgress = 0

        'expected in form: newmtl then map_Kd, consecutively (so that indices end up matching correctly)
        For i = 0 To fileContents.Length - 1
            If Left(fileContents(i), 6) = "newmtl" Then 'get name of material to add to matList, integer index = matList.Length (before added, so is equal to correct index => no items = 0)
                Dim line() As String = Split(fileContents(i), " ", 2) 'in form: "newmtl" "_name"
                Dim name As String = line(1)
                Dim index As Integer = matList.Count
                matList.Add(New Tuple(Of String, Integer)(name, index))

                'check if no map_Kd before second newmtl or beyond (don't check first)
                If mCount > 0 Then
                    If tCount <> mCount Then
                        'test if no map_Kd, and if so, load in default textures with warning (only a warning, because some materials do not always need textures associated with them)
                        'get bin directory, then the parent of that (which will store the "Resources" folder which contains the ui elements)
                        If tCount <> mCount Then
                            Dim base As String = Directory.GetCurrentDirectory
                            Dim splits() As String = base.Split("\")
                            While splits(splits.Length - 1) <> "bin"
                                base = Directory.GetParent(base).ToString
                                splits = base.Split("\")
                            End While
                            base = Directory.GetParent(base).ToString & "\Resources"
                            Dim defaultTex = base & "\Untextured.png"
                            LoadImg(de, tList, sList, format, defaultTex)
                            tNotFound = True
                            tCount += 1
                        End If
                    End If
                End If

                mCount += 1
            ElseIf Left(fileContents(i), 6) = "map_Kd" Then 'load texture and add it to texList
                Dim line() As String = Split(fileContents(i), " ", 2)
                Dim texPath() As String = Split(line(1), "\") 'gets full filepath
                Dim tFileName As String = ""
                Dim dPath As String = ""

                For j = 0 To texPath.Length - 1 'get rid of duplicate "\"
                    If texPath(j).Length > 0 Then
                        tFileName = tFileName & "\" & texPath(j)
                        dPath = line(1)
                    End If
                Next
                tFileName = filePath & tFileName

                'loading in image data separately, in a subroutine, per image (rather than saving the bitmaps) helps reduce memory usage
                'since the wpf bitmap references unmanaged memory which will be claimed quicker at the end of a subroutine when out of scope
                Try
                    LoadImg(de, tList, sList, format, tFileName)
                Catch 'if this doesn't work, try using file name as a direct path
                    LoadImg(de, tList, sList, format, dPath)
                End Try
                tCount += 1
            ElseIf fileContents(i)(0) = "d"c Then 'd for transparency
                tr.Add(CSng(Split(fileContents(i), " ", 2)(1)))
            End If
            currentProgress += progress
            If currentProgress >= 1 OrElse i = fileContents.Length - 1 Then
                de.Dispatcher.Invoke(Sub()
                                         de.LoadBar.Value += currentProgress
                                         de.LoadBar.InvalidateVisual()
                                     End Sub)
            End If
        Next

        'test if no map_Kd, and if so, load in default textures with warning
        'get bin directory, then the parent of that (which will store the "Resources" folder which contains the ui elements)
        If tCount <> mCount Then
            Dim base As String = Directory.GetCurrentDirectory
            Dim splits() As String = base.Split("\")
            While splits(splits.Length - 1) <> "bin"
                base = Directory.GetParent(base).ToString
                splits = base.Split("\")
            End While
            base = Directory.GetParent(base).ToString & "\Resources"
            Dim defaultTex = base & "\Untextured.png"
            LoadImg(de, tList, sList, format, defaultTex)
            tNotFound = True
        End If
        If tNotFound Then
            MessageBox.Show("Warning: Textures may be missing. File may be corrupt")
        End If

        matContents = matList.ToArray()
        transparency = tr.ToArray

        texture = tList.ToArray
        texSize = sList.ToArray

        tList = Nothing
        sList = Nothing
        matList = Nothing
    End Sub

    Shared Sub LoadImg(de As DevelopmentEnvironment, tList As List(Of Integer()), sList As List(Of Vector2), format As PixelFormat, tFileName As String)
        'load in texture
        Dim fileSize As Long = FileLen(tFileName)
        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Maximum += fileSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)

        Dim temp As New FormatConvertedBitmap

        temp.BeginInit()
        temp.Source = New BitmapImage(New Uri(tFileName))
        temp.DestinationFormat = format
        temp.EndInit()

        temp.Source.Freeze()
        temp.Freeze()

        Dim bpp As Integer = format.BitsPerPixel / 8 'get in bytes

        Dim tempT((temp.PixelWidth + 1) * (temp.PixelHeight + 1)) As Integer
        tList.Add(tempT)

        temp.CopyPixels(tList(tList.Count - 1), bpp * temp.PixelWidth, 0)
        sList.Add(New Vector2(temp.PixelWidth, temp.PixelHeight)) 'only need to save height because all bitmaps of same pixel format, so stride is the same

        de.Dispatcher.Invoke(Sub()
                                 de.LoadBar.Value += fileSize
                                 de.LoadBar.InvalidateVisual()
                             End Sub)
    End Sub
End Class
