Module NetworkMaths
    Public Function Sigmoid(ByVal z As Double) As Double
        Return 1 / (1 + Math.Exp(-z))
    End Function

    Public Function Sigmoid(ByVal z As Vector) As Vector
        Dim vec As New Vector(z.numberOfRows, FillTypes.None)
        For i = 0 To z.numberOfRows - 1
            vec.Values(i) = Sigmoid(z.Values(i))
        Next
        Return vec
    End Function

    Public Function Logit(ByVal z As Double) As Double
        Return -Math.Log(1 / z - 1)
    End Function

    Public Function Logit(ByVal z As Vector) As Vector
        Dim vec As New Vector(z.numberOfRows, FillTypes.None)
        For i = 0 To z.numberOfRows - 1
            vec.Values(i) = Logit(z.Values(i))
        Next
        Return vec
    End Function

    Public Function SigmoidDerivatve(ByVal z As Double) As Double
        Dim sig As Double = Sigmoid(z)
        Return sig * (1 - sig)
    End Function

    Public Function SigmoidDerivatve(ByVal z As Vector) As Vector
        Dim vec As New Vector(z.numberOfRows, FillTypes.None)
        For i = 0 To z.numberOfRows - 1
            vec.Values(i) = SigmoidDerivatve(z.Values(i))
        Next
        Return vec
    End Function

    Public Function MSEDerivative(outputActivations As Vector, desiredOutput As Vector)
        Return outputActivations - desiredOutput
    End Function

    Public Function CrossEntropyDerivative(outputActivations As Vector, desiredOutput As Vector)
        Return outputActivations - desiredOutput
    End Function

End Module
