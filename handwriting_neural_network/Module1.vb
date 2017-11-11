Imports System.IO

Module Module1

    'uses box-muller transform
    Public Function generateGaussianRnd(ByVal mu As Double, ByVal sigma As Double) As Double
        Dim z1 As Double
        Static z2 As Double
        Static generate As Boolean = True
        If generate Then
            Dim x1, x2 As Single
            x1 = Rnd()
            x2 = Rnd()
            z1 = Math.Sqrt(-2 * Math.Log(x1)) * Math.Cos(2 * Math.PI * x2)
            z2 = Math.Sqrt(-2 * Math.Log(x1)) * Math.Sin(2 * Math.PI * x2)
            generate = False
            Return z1
        Else
            generate = True
            Return z2
        End If
    End Function

    Public Sub writeSuccessRate(ByRef successRate() As Double)
        Static count As Integer = 0
        Dim sw As StreamWriter = New StreamWriter("success.csv", True)
        For i = 0 To successRate.Length - 1
            sw.Write(i + count * (successRate.Length - 1))
            sw.Write(",")
            sw.Write(Math.Round(successRate(i), 3))
            sw.Write(vbNewLine)

        Next
        sw.Close()

        Dim blank(successRate.Length - 1) As Double
        successRate = blank
        count += 1
    End Sub

    'Not my code
    Sub Shuffle(Of T)(ByRef array As Array)
        Dim r As Random = New Random()
        For i = 0 To array.Length - 1
            Dim index As Integer = r.Next(i, array.Length)
            If i <> index Then
                ' swap list(i) and list(index)
                Dim temp As T = array(i)
                array(i) = array(index)
                array(index) = temp
            End If
        Next
    End Sub

    Public Function SubSet(Of T)(ByVal array() As T, ByVal startIndex As Integer, ByVal size As Integer) As T()
        Dim newArray(size - 1) As T
        For i = startIndex To startIndex + size - 1
            newArray(i - startIndex) = array(i)
        Next
        Return newArray
    End Function

    Public Function CopyOf(Of T)(ByVal array() As T) As T()
        Dim copy(array.Length - 1) As T
        For i = 0 To array.Length - 1
            copy(i) = array(i)
        Next
        Return copy
    End Function


    Sub Main()

        Console.WriteLine("Running")
        Randomize()
        Dim testNetwork As New Network({784, 50, 50, 10}, FillTypes.RandomSDRootn)
        Console.WriteLine("Network Created")
        Console.WriteLine("Data Loading...")

        Dim datas(1)() As ImageTuple
        Dim trainingData(49999) As ImageTuple
        Dim validationData(9999) As ImageTuple
        datas = LoadData()
        trainingData = datas(0)
        validationData = datas(1)
        Console.WriteLine("Data Loaded")

        'Do
        testNetwork.Train(trainingData, 20, 10, 1, 500, True, True, CostFunctions.CrossEntropy)

        'Dim input As String
        'Do
        '    Console.Write("Enter the number to reverse engineer, enter X to resume training: ")
        '    input = Console.ReadLine()
        '    If input <> "X" Then
        '        testNetwork.ReverseEngineer(CInt(input))
        '    End If
        'Loop Until input = "X"
        'Loop

        Console.ReadLine()
    End Sub

End Module
