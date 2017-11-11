Imports Accord.Math

Module NetworkClasses

    'ToDo: implement early stopping and variable eta for a no-improvement-in-n-epochs scenario

    Public Enum FillTypes As Integer
        Zero
        RandomSD1
        RandomSDRootn
        None
    End Enum

    Public Enum CostFunctions As Integer
        MeanSquaredError
        CrossEntropy
    End Enum

    Public Class Network

        Public ReadOnly numberOfLayers As Integer
        Public ReadOnly numberOfNodes As Integer
        Public ReadOnly layerSizes() As Integer
        Public ReadOnly Property lastLayer As Integer
            Get
                Return numberOfLayers - 1
            End Get
        End Property
        Private _weights As Matrix()
        Private _biases As Vector()
        Public Property Weights() As Matrix()
            Get
                Return _weights
            End Get
            Set(ByVal value As Matrix())
                _weights = value
            End Set
        End Property
        Public Property Biases() As Vector()
            Get
                Return _biases
            End Get
            Set(ByVal value As Vector())
                _biases = value
            End Set
        End Property

        Public Sub New(ByVal sizes() As Integer, Optional weightInit As FillTypes = FillTypes.RandomSD1, Optional biasInit As FillTypes = FillTypes.RandomSD1)

            Me.numberOfLayers = sizes.Length
            Me.layerSizes = sizes

            ReDim Weights(lastLayer)
            ReDim Biases(lastLayer)

            Me.numberOfNodes = 0
            For i = 0 To lastLayer
                Me.numberOfNodes += sizes(i)
            Next

            For layer = 1 To lastLayer
                Me.Weights(layer) = New Matrix(sizes(layer), sizes(layer - 1), weightInit)
                Me.Biases(layer) = New Vector(sizes(layer), biasInit)
            Next

        End Sub

        Private Function WeightPreviousActivations(ByVal prevActivations As Vector, ByVal layerNumber As Integer) As Vector
            If layerNumber = 0 Then
                Console.WriteLine("Error: Trying to feedforward from layer -1")
                Return Nothing
            End If

            Dim weightedInputs As New Vector(layerSizes(layerNumber))
            For neuron = 0 To layerSizes(layerNumber) - 1
                Dim rowOfWeights() As Double = Weights(layerNumber).Values(neuron)
                weightedInputs.Values(neuron) = Vector.Dot(rowOfWeights, prevActivations) + Biases(layerNumber).Values(neuron)
            Next
            Return weightedInputs

        End Function

        Private Function MSE(ByVal outputs() As Vector, ByVal predictions() As Vector, ByVal n As Integer) As Double

            Dim sum As Double = 0

            For i = 0 To n - 1
                Dim differenceVector As New Vector(outputs(0).numberOfRows)
                Dim sumOfSquares As Double = 0
                differenceVector = outputs(i) - predictions(i)
                sum += differenceVector.Magnitude ^ 2
            Next
            Return sum / (2 * n)
        End Function

        Private Function CrossEntropy(ByVal outputs() As Vector, ByVal predictions() As Vector, ByVal n As Integer) As Double
            Dim sum As Double = 0

            For i = 0 To n - 1
                For j = 0 To outputs(0).numberOfRows - 1
                    Dim y As Double = predictions(i).Values(j)
                    Dim a As Double = outputs(i).Values(j)
                    'TODO WHAT DOES THIS DO AT 0
                    sum += (y * Math.Log(a)) + ((1 - y) * Math.Log(1 - a))
                Next
            Next
            Return (-sum) / n
        End Function

        Public Sub Train(ByVal trainingInputs() As ImageTuple, ByVal numberOfEpochs As Integer, ByVal miniBatchSize As Integer, ByVal eta As Double, ByVal outputRate As Single, ByVal ShowCost As Boolean, ByVal ShowSuccess As Boolean, Optional costFunction As CostFunctions = CostFunctions.MeanSquaredError)
            For epochNumber = 0 To numberOfEpochs - 1
                Console.WriteLine("Epoch {0}", epochNumber)

                Dim miniBatches() As Batch
                miniBatches = SplitTrainingData(trainingInputs, miniBatchSize)
                StochasticGradientDescent(miniBatches, miniBatchSize, eta, outputRate, costFunction, ShowCost, ShowSuccess)

            Next epochNumber
        End Sub

        Private Function SplitTrainingData(ByVal trainingInputs() As ImageTuple, ByVal miniBatchSize As Integer) As Batch()
            Dim numberOfInputs As Integer = trainingInputs.Length - 1
            Shuffle(Of ImageTuple)(trainingInputs)

            Dim miniBatches(numberOfInputs \ miniBatchSize) As Batch
            Dim count As Integer = 0
            For batchStartIndex = 0 To numberOfInputs - 1 Step miniBatchSize
                miniBatches(count) = New Batch(miniBatchSize)
                miniBatches(count).Images = SubSet(trainingInputs, batchStartIndex, miniBatchSize)
                count += 1
            Next

            Return miniBatches
        End Function

        Private Sub StochasticGradientDescent(ByVal miniBatches() As Batch, ByVal miniBatchSize As Integer, ByVal eta As Double, ByVal outputRate As Integer, ByVal costFunction As CostFunctions, ByVal ShowCost As Boolean, ByVal ShowSuccess As Boolean)

            Dim outputsOfBatches(miniBatchSize * outputRate - 1) As Vector
            Dim desiredOutputsOfBatches(miniBatchSize * outputRate - 1) As Vector
            Dim numberSuccessfulOfBatches As Integer
            Dim batchCount As Integer = 0

            For i = 0 To miniBatches.Length - 1
                Dim batch As Batch = miniBatches(i)
                UpdateParameters(batch, eta, costFunction)
                numberSuccessfulOfBatches += batch.numberCorrect
                For j = 0 To miniBatchSize - 1
                    outputsOfBatches(batchCount * miniBatchSize + j) = batch.Outputs(j)
                    desiredOutputsOfBatches(batchCount * miniBatchSize + j) = batch.DesiredOutputs(j)
                Next

                batchCount += 1

                If (i + 1) Mod outputRate = 0 Then
                    Console.Write("Batches {0}-{1}: ", (i + 1 - outputRate), (i))
                    If ShowCost Then
                        Dim cost As Double
                        Select Case costFunction
                            Case CostFunctions.MeanSquaredError
                                cost = MSE(outputsOfBatches, desiredOutputsOfBatches, miniBatchSize * outputRate)
                            Case CostFunctions.CrossEntropy
                                cost = CrossEntropy(outputsOfBatches, desiredOutputsOfBatches, miniBatchSize * outputRate)
                        End Select
                        Console.Write("Cost = {0}, ", Math.Round(cost, 3))
                    End If
                    If ShowSuccess Then
                        Console.Write("Success = {0}/{1} = {2}%,", numberSuccessfulOfBatches, outputRate * miniBatchSize, Math.Round((numberSuccessfulOfBatches * 100) / (outputRate * miniBatchSize), 3))
                    End If
                    Console.WriteLine()
                    batchCount = 0
                    numberSuccessfulOfBatches = 0
                End If
            Next
        End Sub

        Private Sub UpdateParameters(ByRef miniBatch As Batch, ByVal eta As Double, ByVal costFunction As CostFunctions)

            Dim nablaB(numberOfLayers - 1) As Vector
            Dim nablaW(numberOfLayers - 1) As Matrix
            For layer = 1 To lastLayer
                nablaB(layer) = New Vector(layerSizes(layer), FillTypes.Zero)
                nablaW(layer) = New Matrix(layerSizes(layer), layerSizes(layer - 1), FillTypes.Zero)
            Next

            'calculates the changes needed for the weights and biases
            For i = 0 To miniBatch.Size - 1
                Dim image As ImageTuple = miniBatch.Images(i)
                Dim deltaNablaB(numberOfLayers - 1) As Vector
                Dim deltaNablaW(numberOfLayers - 1) As Matrix
                Dim deltaNablas(1) As Object
                deltaNablas = Backpropogate(image, miniBatch, i, costFunction)
                deltaNablaB = deltaNablas(0)
                deltaNablaW = deltaNablas(1)
                For layer = 1 To lastLayer
                    nablaB(layer) += deltaNablaB(layer)
                    nablaW(layer) += deltaNablaW(layer)

                Next layer
            Next i

            For layer = 1 To lastLayer
                Biases(layer) = Biases(layer) - (eta / miniBatch.Size) * nablaB(layer)
                Weights(layer) = Weights(layer) - (eta / miniBatch.Size) * nablaW(layer)
            Next layer

        End Sub

        Private Function Backpropogate(ByVal image As ImageTuple, ByRef miniBatch As Batch, ByVal imageIndex As Integer, ByVal costFunction As CostFunctions) As Object()


            'compute dC/dW (nablaWC) and dC/dB (nablaBC)
            'let delta = dC/dz, where z = weighted input to a neuron, delta = 'error' of a particular neuron
            '       large delta means large dC/dz = large error
            '       z is weighted therefore affected by the weights and biases [ z = wa + b ]

            Dim nablaB(numberOfLayers - 1) As Vector
            Dim nablaW(numberOfLayers - 1) As Matrix
            For layer = 1 To lastLayer
                nablaB(layer) = New Vector(layerSizes(layer), FillTypes.Zero)
                nablaW(layer) = New Matrix(layerSizes(layer), layerSizes(layer - 1), FillTypes.Zero)
            Next

            'TODO CHECK THE GRADIENTS ARE BEING AVERAGED

            Dim input As Vector = image.imageVector
            Dim zValues(lastLayer) As Vector
            Dim activations(lastLayer) As Vector
            Dim delta(lastLayer) As Vector

            'STEP 1: input a0
            activations(0) = input

            'STEP 2: feedforward
            For layer = 1 To numberOfLayers - 1
                zValues(layer) = WeightPreviousActivations(activations(layer - 1), layer)
                activations(layer) = Sigmoid(zValues(layer))
            Next layer
            If activations(lastLayer).IndexOfHighestValue = image.desiredOutput.IndexOfHighestValue Then
                miniBatch.numberCorrect += 1
            End If
            miniBatch.DesiredOutputs(imageIndex) = image.desiredOutput
            miniBatch.Outputs(imageIndex) = activations(lastLayer)

            'STEP 3: compute delta(Last)
            Select Case costFunction
                Case CostFunctions.MeanSquaredError
                    delta(lastLayer) = Vector.HadamardProduct(MSEDerivative(activations(lastLayer), image.desiredOutput),
                                                          SigmoidDerivatve(zValues(lastLayer)))
                Case CostFunctions.CrossEntropy
                    delta(lastLayer) = CrossEntropyDerivative(activations(lastLayer), image.desiredOutput)
            End Select
            nablaB(lastLayer) = delta(lastLayer)
            For k = 0 To layerSizes(lastLayer) - 1
                For j = 0 To layerSizes(lastLayer - 1) - 1
                    nablaW(lastLayer).Values(k)(j) = activations(lastLayer - 1).Values(j) * delta(lastLayer).Values(k)
                Next
            Next

            'STEP 4: backpropagate the error
            For layer = lastLayer - 1 To 1 Step -1

                delta(layer) = Vector.HadamardProduct(Weights(layer + 1).Transpose * delta(layer + 1), SigmoidDerivatve(zValues(layer)))
                nablaB(layer) = delta(layer)
                For k = 0 To layerSizes(layer) - 1
                    For j = 0 To layerSizes(layer - 1) - 1
                        nablaW(layer).Values(k)(j) = activations(layer - 1).Values(j) * delta(layer).Values(k)
                    Next
                Next
            Next

            'STEP 5: return dC/db and dC/dw
            Return {nablaB, nablaW}
        End Function

        'Public Sub ReverseEngineer(ByVal number As Integer)
        '    Dim pixels As New Vector(783)
        '    Dim input As New Vector(10, FillTypes.Zero)
        '    Dim activations(lastLayer) As Vector

        '    activations(lastLayer) = input
        '    For layer = numberOfLayers - 1 To 1 Step -1
        '        activations(layer - 1) = Weights(layer).Inverse * (Logit(activations(layer)) - Biases(layer))
        '    Next

        '    DrawImage(activations(0))

        'End Sub

    End Class

    Private Class Batch

        Private _numberCorrect As Integer
        Public Property numberCorrect() As Integer
            Get
                Return _numberCorrect
            End Get
            Set(ByVal value As Integer)
                _numberCorrect = value
            End Set
        End Property

        Private _outputs As Vector()
        Public Property Outputs() As Vector()
            Get
                Return _outputs
            End Get
            Set(ByVal value As Vector())
                _outputs = value
            End Set
        End Property

        Private _desiredOutputs As Vector()
        Public Property DesiredOutputs() As Vector()
            Get
                Return _desiredOutputs
            End Get
            Set(ByVal value As Vector())
                _desiredOutputs = value
            End Set
        End Property

        Public ReadOnly Property Size As Integer
            Get
                Return _images.Length
            End Get
        End Property

        Private _images As ImageTuple()
        Public Property Images() As ImageTuple()
            Get
                Return _images
            End Get
            Set(ByVal value As ImageTuple())
                _images = value
            End Set
        End Property

        Sub New(ByVal size As Integer)
            ReDim _images(size - 1)
            ReDim _outputs(size - 1)
            ReDim _desiredOutputs(size - 1)
        End Sub

    End Class

    Public Class Matrix

        Public ReadOnly numberOfRows As Integer
        Public ReadOnly numberOfColumns As Integer

        Public ReadOnly Property Transpose As Matrix
            Get
                Dim transposition As New Matrix(Me.numberOfColumns, Me.numberOfRows)

                For i = 0 To Me.numberOfRows - 1
                    For j = 0 To Me.numberOfColumns - 1
                        transposition.Values(j)(i) = Me.Values(i)(j)
                    Next
                Next
                Return transposition
            End Get
        End Property

        Private _values As Double()() ' = New Double()() {}
        Public Property Values() As Double()()
            Get
                Return _values
            End Get
            Set(ByVal value As Double()())
                _values = value
            End Set
        End Property

        Public ReadOnly Property Inverse As Matrix
            Get
                Dim inv As New Matrix(Me.numberOfColumns, Me.numberOfRows)
                inv._values = _values.Inverse()
                Return inv
            End Get
        End Property

        Sub New(ByVal rows As Integer, ByVal columns As Integer, Optional fill As FillTypes = FillTypes.None)

            Me.numberOfRows = rows
            Me.numberOfColumns = columns

            ReDim _values(numberOfRows - 1)
            For i = 0 To numberOfRows - 1
                ReDim _values(i)(numberOfColumns - 1)
            Next

            For i = 0 To rows - 1
                For j = 0 To columns - 1
                    Select Case fill
                        Case FillTypes.Zero
                            Me.Values(i)(j) = 0
                        Case FillTypes.RandomSD1
                            Me.Values(i)(j) = generateGaussianRnd(0, 1)
                        Case FillTypes.RandomSDRootn
                            Me.Values(i)(j) = generateGaussianRnd(0, 1 / Math.Sqrt(Me.numberOfColumns))
                    End Select
                Next
            Next
        End Sub

        Public Shared Operator -(ByVal mat1 As Matrix, ByVal mat2 As Matrix) As Matrix

            If mat1.numberOfColumns <> mat2.numberOfColumns Or mat1.numberOfRows <> mat2.numberOfRows Then
                Console.WriteLine("Error: attempting to add 2 matrices with different dimentions")
                Return Nothing
            End If

            Dim ans As New Matrix(mat1.numberOfRows, mat1.numberOfColumns)
            For i = 0 To ans.numberOfRows - 1
                For j = 0 To ans.numberOfColumns - 1
                    ans.Values(i)(j) = mat1.Values(i)(j) - mat2.Values(i)(j)
                Next
            Next
            Return ans
        End Operator

        Public Shared Operator +(ByVal mat1 As Matrix, ByVal mat2 As Matrix) As Matrix

            If mat1.numberOfColumns <> mat2.numberOfColumns Or mat1.numberOfRows <> mat2.numberOfRows Then
                Console.WriteLine("Error: attempting to add 2 matrices with different dimentions")
                Return Nothing
            End If

            Dim ans As New Matrix(mat1.numberOfRows, mat1.numberOfColumns)
            For i = 0 To ans.numberOfRows - 1
                For j = 0 To ans.numberOfColumns - 1
                    ans.Values(i)(j) = mat1.Values(i)(j) + mat2.Values(i)(j)
                Next
            Next
            Return ans
        End Operator

        Public Shared Operator *(ByVal scalar As Double, ByVal matrix As Matrix) As Matrix

            Dim ans As New Matrix(matrix.numberOfRows, matrix.numberOfColumns)
            For i = 0 To ans.numberOfRows - 1
                For j = 0 To ans.numberOfColumns - 1
                    ans.Values(i)(j) = matrix.Values(i)(j) * scalar
                Next
            Next
            Return ans
        End Operator

        'Public Shared Operator *(ByVal mat1 As Matrix, ByVal mat2 As Matrix) As Matrix

        '    If mat1.numberOfColumns <> mat2.numberOfRows Then
        '        Console.WriteLine("Error: attempting to multiply 2 matrices with uncompatible dimentions")
        '        Return Nothing
        '    End If

        '    Dim ans As New Matrix(mat1.numberOfRows, mat2.numberOfColumns)


        '    Return ans
        'End Operator

        Public Shared Operator *(ByVal matrix As Matrix, ByVal vector As Vector) As Vector

            If matrix.numberOfColumns <> vector.numberOfRows Then
                Console.WriteLine("Error: attempting to multiply a matrix and a vector with uncompatible dimentions")
                Return Nothing
            End If

            Dim ans As New Vector(matrix.numberOfRows)
            For i = 0 To matrix.numberOfRows - 1
                ans.Values(i) = vector.Dot(matrix.Values(i), vector)
            Next
            Return ans
        End Operator

    End Class

    Public Class Vector

        Public ReadOnly numberOfRows As Integer

        Private _values As Double() '= New Double() {}
        Public Property Values() As Double()
            Get
                Return _values
            End Get
            Set(ByVal value As Double())
                _values = value
            End Set
        End Property

        Public ReadOnly Property Magnitude() As Double
            Get
                Dim sumOfSquares As Double = 0
                For i = 0 To numberOfRows - 1
                    sumOfSquares += Values(i) ^ 2
                Next
                Return Math.Sqrt(sumOfSquares)
            End Get
        End Property

        Public ReadOnly Property IndexOfHighestValue As Integer
            Get
                Dim largest As Integer = 0
                For i = 0 To numberOfRows - 1
                    If Values(i) > Values(largest) Then
                        largest = i
                    End If
                Next
                Return largest
            End Get
        End Property

        Sub New(ByVal rows As Integer, Optional fill As FillTypes = FillTypes.None)
            Me.numberOfRows = rows

            ReDim _values(numberOfRows - 1)

            For i = 0 To rows - 1
                Select Case fill
                    Case FillTypes.Zero
                        Me.Values(i) = 0
                    Case FillTypes.RandomSD1
                        Me.Values(i) = generateGaussianRnd(0, 1)
                    Case FillTypes.RandomSDRootn
                        Console.WriteLine("RandomSDRootn not applicable for biases")
                        Me.Values(i) = generateGaussianRnd(0, 1)
                End Select
            Next
        End Sub

        Public Shared Operator -(ByVal vec1 As Vector, ByVal vec2 As Vector) As Vector
            If vec1.numberOfRows <> vec2.numberOfRows Then
                Console.WriteLine("Error: attempting to subtract two vectors with unequal dimentions")
                Return Nothing
            End If
            Dim ansVec As New Vector(vec1.numberOfRows)
            For i = 0 To vec1.numberOfRows - 1
                ansVec.Values(i) = vec1.Values(i) - vec2.Values(i)
            Next
            Return ansVec
        End Operator

        Public Shared Operator +(ByVal vec1 As Vector, ByVal vec2 As Vector) As Vector
            If vec1.numberOfRows <> vec2.numberOfRows Then
                Console.WriteLine("Error: attempting to add two vectors with unequal dimentions")
                Return Nothing
            End If
            Dim ansVec As New Vector(vec1.numberOfRows)
            For i = 0 To vec1.numberOfRows - 1
                ansVec.Values(i) = vec1.Values(i) + vec2.Values(i)
            Next
            Return ansVec
        End Operator

        Public Shared Operator *(ByVal scalar As Double, ByVal vec As Vector) As Vector
            Dim ans As New Vector(vec.numberOfRows)
            For i = 0 To vec.numberOfRows - 1
                ans.Values(i) = vec.Values(i) * scalar
            Next
            Return ans
        End Operator

        Public Shared Function Dot(ByVal vector1() As Double, ByVal vector2() As Double) As Double
            Dim sum As Double
            If vector1.Length <> vector2.Length Then
                Console.WriteLine("Error: attempting to dot two vectors with unequal dimentions")
                Return Nothing
            End If
            For i = 0 To vector1.Length - 1
                sum += vector1(i) * vector2(i)
            Next
            Return sum
        End Function

        Public Shared Function Dot(ByVal vector1() As Double, ByVal vector2 As Vector) As Double
            Dim sum As Double
            If vector1.Length <> vector2.numberOfRows Then
                Console.WriteLine("Error: attempting to dot two vectors with unequal dimentions")
                Return Nothing
            End If
            For i = 0 To vector1.Length - 1
                sum += vector1(i) * vector2.Values(i)
            Next
            Return sum
        End Function

        Public Shared Function Dot(ByVal vector1 As Vector, ByVal vector2 As Vector) As Double
            Dim sum As Double
            If vector1.numberOfRows <> vector2.numberOfRows Then
                Console.WriteLine("Error: attempting to dot two vectors with unequal dimentions")
                Return Nothing
            End If
            For i = 0 To vector1.numberOfRows - 1
                sum += vector1.Values(i) * vector2.Values(i)
            Next
            Return sum
        End Function

        Public Shared Function Dot(ByVal vector1 As Vector, ByVal vector2() As Double) As Double
            Dim sum As Double
            If vector1.numberOfRows <> vector2.Length Then
                Console.WriteLine("Error: attempting to dot two vectors with unequal dimentions")
                Return Nothing
            End If
            For i = 0 To vector1.numberOfRows - 1
                sum += vector1.Values(i) * vector2(i)
            Next
            Return sum
        End Function

        Public Shared Function HadamardProduct(ByVal vec1 As Vector, ByVal vec2 As Vector) As Vector
            If vec1.numberOfRows <> vec2.numberOfRows Then
                Return Nothing
            End If
            Dim product As New Vector(vec1.numberOfRows)
            For i = 0 To vec1.numberOfRows - 1
                product.Values(i) = vec1.Values(i) * vec2.Values(i)
            Next
            Return product
        End Function

    End Class

End Module
