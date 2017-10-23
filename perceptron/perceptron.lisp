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
;;     - inputs: Lista contendo as entradas, é uma lista onde cada elemento é uma outra lista, onde esta última contém 
;;      entradas. O primeiro input é contado como o bias.
;;     - desired-responses: lista contendo, em ordem, as respostas esperadas para cada combinação de entradas
;;     contida em inputs.
;;          Optativos:
;;     - learning-rate: parâmetro que define a taxa de aprendizado, deve ser na faixa 0 <= learning-rate <= 1.
;;     - signum-function: a função limitadora de saídas. Deve receber como parâmetro uma única variável v. Por padrão é
;;             +1 -> v > 0
;;             -1 -> v <= 0
;;         Onde v é a combinação linear das entradas*pesos.
;;     - max-reps: numero máximo de repetições, padrão é 1000. Serve para impedir um loop infinito em problemas
;;     não-linearmente separáveis.
;;     Retorna:
;;         Uma lista contendo os pesos para a solução do problema, na ordem de suas entradas correspondentes no parâmetro input.
;;         O peso do bias é retornado como a primeira entrada, seguida pelos outros pesos.
(defun perceptron-adapt (inputs desired-responses &optional &key (learning-rate 0.1) (signum-function nil) (max-reps 1000))
    (let ((new-weights) (s-func))
        (setf new-weights (make-list (list-length (first inputs)) :initial-element 1))
        (if (null signum-function)
            (setf s-func (lambda (v) (if (<= v 0) 0 1)))
            (setf s-func signum-function))
        (dotimes (n max-reps) 
            (let ((has-error))
                (setf has-error nil)
                (loop for yd in desired-responses and current-input in inputs do
                    (let ((va) (err) (ya))
                        (setf va (linear-combiner current-input new-weights))
                        (setf ya (funcall s-func va))
                        (if (not (= ya yd))
                            (progn
                                (setf has-error t)
                                (setf err (error-calc va yd ya))
                                (loop for wa in new-weights and xa in current-input and index from 0 do
                                    (let ((wn))
                                        (setf wn (weight-adapt wa learning-rate err xa))
                                        (setf (nth index new-weights) wn)))))))
                (when (not has-error)
                    (return new-weights))))))

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
        