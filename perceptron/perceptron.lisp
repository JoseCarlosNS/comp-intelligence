(defstruct perceptron
    weights
    num-inputs
    signum-function)


;; Função que retorna a saída do perceptron dado certas entradas
;;     Parâmetros:
;;         p: a estrutura do perceptron.
;;         i: as entradas atuais.
;;     Retorna:
;;         O valor de saída do perceptron para as entradas atuais.
(defun perceptron (p i)
    (let ((linear-comb))
        (setf linear-comb (linear-combiner i (perceptron-weights p)))
        (funcall (eval (perceptron-signum-function p)) linear-comb)))

;; Dado os parâmetros o algoritmo executa o "Perceptron Convergence Algorithm" e retorna uma lista com os pesos
;; na ordem das suas respectivas entradas, conforme passado como parâmetro.
;;     Parâmetros
;;         -num-inputs: número de entradas.
;;         -biasp: t para utilizar o bias, senão nil
;;         -input-values: os valores que as entradas podem tomar
;;         -inputs: A lista onde cada elemento é uma lista de valores de entradas.
;;         -desired-responses: uma lista contendo pares (x y), onde:
;;             x: uma combinação de entradas
;;             y: a saída desejada, na ocorrência de x
;;         -default-response: a saída para todas as outras combinações de entradas não presentes em
;;         desired-responses.
;;             Optativos:
;;         - learning-rate: parâmetro que define a taxa de aprendizado, deve ser na faixa 0 <= learning-rate <= 1.
;;         - signum-function: a função de limitação de saída no formato f(x) = y, onde:
;;             y = -1 -> caso x <= 0
;;             y = 1 -> caso x > 0
;;         - max-n: o número de repetições máxima para o algoritmo, caso o número de repetições exceda o máximo, a função
;;         retorna nil. Isto serve para impedir loops infinitos em problemas cujas saídas desejáveis não são linearmente
;;         separáveis. Por padrão são 1000 repetições máximas
;;     Retorna:
;;         Uma lista contendo os pesos para a solução do problema, na ordem de suas entradas correspondentes.
;;         A lista conterá tamanho (num-inputs + 1), caso biasp = t, onde temos o peso adicional do bias, no primeiro índice.
(defun perceptron-conv (num-inputs biasp input-values desired-responses default-response &optional &key (learning-rate 0.1) (signum-function nil) (max-n 1000))
    (let ((weights) (indexes) (num-values))
        (setf weights (make-list (if biasp (+ 1 num-inputs) num-inputs) :initial-element 1))
        (setf num-values (list-length input-values))
        (setf indexes (make-list num-inputs :initial-element 0))
        (when (not signum-function)
            (setf signum-function (lambda (v) (if (<= v 0) -1 1))))
        (dotimes (n max-n) 
            (let ((has-error))
                (setf has-error nil)
                (loop for x from 1 to (expt num-values num-inputs) do
                    (let ((input) (new-indexes) (va) (err) (ya) (yd))   
                        (loop for e in indexes and i from 0 do
                            (push (nth e input-values) input)
                            (if (= 0 (rem x (expt num-values i)))
                                (push (rem (+ 1 e) num-values) new-indexes)
                                (push e new-indexes)))
                        (setf indexes (reverse new-indexes))
                        (when biasp (push 1 input))
                        (setf va (linear-combiner input weights))
                        (setf ya (funcall signum-function va))
                        (setf yd default-response)
                        (dolist (e desired-responses)
                            (when (equal input (first e))
                                (return (setf yd (second e)))))
                        (when (not (= ya yd))
                            (progn
                                (setf has-error t)
                                (setf err (error-calc va yd ya))
                                (let ((new-weights))
                                    (loop for wa in weights and xa in input do
                                        (let ((wn))
                                            (setf wn (weight-adapt wa learning-rate err xa))
                                            (push wn new-weights)))
                                    (setf weights (reverse new-weights)))))))
                (when (not has-error)
                    (return-from perceptron-conv weights)))))
    nil)

;; Calcula um novo peso dado os parâmetros passados para a função. O peso é calculado utilizando a fórmula de adaptação
;; de pesos do perceptron.
;;     Parâmetros:
;;         - w: o peso atual
;;         - n: a taxa de aprendizado
;;         - e: o erro
;;         - x: a entrada correspondente ao peso
;;     Retorna:
;;         O novo peso calculado.
(defun weight-adapt (w n e x)
    (+ w (* n e x)))

;; Dado que 'v' é a combinação linear das entradas e pesos, calcula o erro dado o 'v' atual, o 'v' desejado e a saída.
;;     Parâmetros
;;         - va: o v atual
;;         - yd: o v desejado
;;         - y: a saída atual
;;     Retorna
;;         O valor do erro, que é utilizado para reajustar os pesos
(defun error-calc (va yd y)
    (- (- yd va) Y))

;; Função que faz a combinação linear da multiplicação das entradas e seus respectivos pesos.
;;     Parâmetros 
;;         - inputs: uma lista de entradas.
;;         - weights: uma lista de pesos.
;;     Retorna
;;         A combinação linear de cada elemento da lista inputs com cada elemento da lista weights.
(defun linear-combiner (inputs weights)
    (let ((combination))
        (setf combination 0)
        (loop for xi in inputs and wi in weights do
            (setf combination (+ combination (* xi wi))))
        combination))
