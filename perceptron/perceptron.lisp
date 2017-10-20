;; Dado os parâmetros o algoritmo executa o "Perceptron Convergence Algorithm" e retorna uma lista com os pesos
;; na ordem das suas respectivas entradas, conforme passado como parâmetro. Vários comandos para imprimir textos no terminal,
;; para motivos de acompanhamento do algoritmo, foram inseridos ao longo da função, para ativá-los basta 'descomentar'.
;;     Parâmetros
;;         -inputs: uma lista onde o tamanho define o número de entradas, e cada elemento contém uma lista de valores
;;         possíveis a serem tomados pela entrada correspondente àquele índice. Caso deseje usar o bias, coloque-o como uma
;;         entrada na lista que contém somente o 1, o que indica uma entrada constante.
;;         - desired-responses: uma lista onde cada elemento é uma combinação de entradas onde a saída desejada é 1. todas
;;         as outras combinações de entradas não pertencentes à esta lista terão como saída default-response.
;;             Optativos:
;;         - weights: uma lista contendo os pesos iniciais do algoritmo. Inicialmente todos os pesos são setados em 1 por 
;;         padrão.
;;         - learning-rate: parâmetro que define a taxa de aprendizado, deve ser na faixa 0 <= learning-rate <= 1.
;;         - signum-function: a função limitadora de saídas. Deve receber como parâmetro uma única variável v. Por padrão é
;;                 +1 -> v > 0
;;                 -1 -> v <= 0
;;             Onde v é a combinação linear das entradas*pesos.
;;         - max-n: o número de repetições máxima para o algoritmo, caso o número de repetições exceda o máximo, a função
;;         retorna nil. Isto serve para impedir loops infinitos em problemas cujas saídas desejáveis não são linearmente
;;         separáveis. Por padrão são 1000 repetições máximas
;;     Retorna:
;;         Uma lista contendo os pesos para a solução do problema, na ordem de suas entradas correspondentes no parâmetro input.
;;         O peso do bias é retornado como a primeira entrada, seguida pelos outros pesos.
(defun perceptron (inputs desired-responses &optional &key (weights nil) (learning-rate 0.1) (signum-function nil) (max-n 1000) (default-response -1))
    (let ((new-weights) (s-func) (n) (input-comb) (default-resp))
        (if (null weights)
            (progn 
                (setf new-weights (make-list (list-length inputs) :initial-element 1))
                (setf (first new-weights) 0)            )
            (setf new-weights weights))
        (if (null signum-function)
            (setf s-func (lambda (v) (if (<= v 0) -1 1)))
            (setf s-func signum-function))
        (setf input-comb (input-combinations inputs))
        (setf n 0)
        (loop 
            (let ((has-error))
                (setf has-error nil)
                (dolist (combination input-comb)
                    (let ((va) (err) (ya) (yd))
                        (setf va (linear-combiner combination new-weights))
                        (setf ya (funcall s-func va))
                        (setf yd default-response)
                        (dolist (e desired-responses)
                            (if (equal combination e)
                                (return (setf yd (* default-response (- 0 1)))))
                        (if (not (= ya yd))
                            (progn
                                (setf n (+ n 1))
                                (setf has-error t)
                                (setf err (error-calc va yd ya))
                                (loop for wa in new-weights and xa in combination and index from 0 do
                                    (let ((wn))
                                        (setf wn (weight-adapt wa learning-rate err xa))
                                        (setf (nth index new-weights) wn)))))))
                (when (not has-error)
                    (return-from perceptron new-weights))
                (when (> n max-n)
                    (return-from perceptron nil))))))

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

;; Função que cria uma lista contendo todas as combinações possíveis dado a lista de entradas passada como
;; parâmetro
;;     Parâmetros  
;;         -inputs: uma lista onde o tamanho define o número de entradas, e cada elemento contém uma lista de valores
;;         possíveis a serem tomados pela entrada correspondente àquele índice.
;;     Retorna
;;         Uma lista contendo todas as combinações possíveis de entradas.
(defun input-combinations (inputs)
    (let ((combination-num) (combinations) (change-rate-list) (index-list))
        (setf change-rate-list (make-list (list-length inputs)))
        (setf combination-num 1)
        (loop for e in inputs and index from 0 do
            (setf combination-num (* combination-num (list-length e)))
            (setf (nth index change-rate-list) combination-num))
        (setf change-rate-list (reverse change-rate-list))
        (setf index-list (make-list (list-length inputs) :initial-element 0))
        (dotimes (a combination-num)
            (let ((combination))
                (setf combination (make-list (list-length inputs)))
                (loop for e in inputs and b from 0 do
                    (let ((change-rate) (e2))
                        (setf change-rate (nth b change-rate-list))
                        (setf e2 (nth b index-list))
                        (if (= 0 (rem a change-rate))
                            (setf (nth b index-list) (rem (+ 1 e2) (list-length e))))
                        (setf (nth b combination) (nth e2 e))))
                (push combination combinations)))
        combinations))
