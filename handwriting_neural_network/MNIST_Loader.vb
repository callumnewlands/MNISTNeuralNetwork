Imports System.IO.Compression
Imports System.IO

Module MNIST_Loader

    Structure ImageTuple
        'vector of pixel values
        Public imageVector As Vector
        Public desiredOutput As Vector
    End Structure



    'returns:
    '   {0} = trainingData
    '   {1} = validationData
    Public Function LoadData() As ImageTuple()()
        Dim trainingData(49999) As ImageTuple
        Dim validationData(9999) As ImageTuple
        'Dim testData(9999) As ImageTuple

        Dim imageStream As FileStream = New FileStream("trainingData\train-images-idx3-ubyte.gz", FileMode.Open)
        Dim gzImageStream As GZipStream = New GZipStream(imageStream, CompressionMode.Decompress)
        Dim brImages As BinaryReader = New BinaryReader(gzImageStream)

        Dim labelStream As FileStream = New FileStream("trainingData\train-labels-idx1-ubyte.gz", FileMode.Open)
        Dim gzLabelStream As GZipStream = New GZipStream(labelStream, CompressionMode.Decompress)
        Dim brLabels As BinaryReader = New BinaryReader(gzLabelStream)

        'discarded data
        Dim magicNoImages As Integer = brImages.ReadInt32()
        Dim numberOfImages As Integer = brImages.ReadInt32()
        Dim numberOfRows As Integer = brImages.ReadInt32()
        Dim numberOfColumns As Integer = brImages.ReadInt32()
        Dim magicNoLabels As Integer = brLabels.ReadInt32()
        Dim numberOfLabels As Integer = brLabels.ReadInt32()

        For imageNo = 0 To 49999
            If (imageNo + 1) Mod 5000 = 0 Then
                Console.WriteLine("{0}/50000 images loaded", imageNo + 1)
            End If
            trainingData(imageNo).imageVector = New Vector(784)
            trainingData(imageNo).desiredOutput = New Vector(10, FillTypes.Zero)
            For i = 0 To 27
                For j = 0 To 27
                    trainingData(imageNo).imageVector.Values(28 * i + j) = (brImages.ReadByte() / 255)
                    'Select Case byteArray(i)(j)
                    '    Case Is > 170
                    '        Console.Write("O")
                    '    Case Is < 85
                    '        Console.Write(" ")
                    '    Case Else
                    '        Console.Write("-")
                    'End Select
                Next
                'Console.WriteLine()
            Next
            Dim label As Byte
            label = brLabels.ReadByte()
            trainingData(imageNo).desiredOutput.Values(label) = 1
        Next

        For imageNo = 0 To 9999
            validationData(imageNo).imageVector = New Vector(784)
            validationData(imageNo).desiredOutput = New Vector(10, FillTypes.Zero)
            For i = 0 To 27
                For j = 0 To 27
                    validationData(imageNo).imageVector.Values(28 * i + j) = (brImages.ReadByte() / 255)
                Next
            Next
            Dim label As Byte
            label = brLabels.ReadByte()
            validationData(imageNo).desiredOutput.Values(label) = 1
        Next

        imageStream.Close()
        labelStream.Close()
        brImages.Close()
        brLabels.Close()
        gzImageStream.Close()
        gzLabelStream.Close()

        Return {trainingData, validationData}
    End Function

    Public Sub DrawImage(ByVal imageVec As Vector)
        For i = 0 To 27
            For j = 0 To 27
                Select Case imageVec.Values(28 * i + j)
                    Case Is > 170
                        Console.Write("O")
                    Case Is < 85
                        Console.Write(" ")
                    Case Else
                        Console.Write("-")
                End Select
            Next
            Console.WriteLine()
        Next
    End Sub

End Module
