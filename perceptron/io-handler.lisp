
;; Função que retorna uma base com todas as combinações não-repetidas de entradas
;;     Parâmetros
;;         input-number: número de entradas
;;         biasp: t para inserir o biasp
;;         input-values: lista com os valores possíveis de entradas
;;     Retorna:
;;         Uma lista onde cada elemento é uma das possíveis combinações de entradas
(defun generate-input (input-number biasp input-values)
    (let ((generated-input) (current-values) (values-num))
        (setf values-num (list-length input-values))
        (setf current-values (make-list input-number :initial-element 0))
        (loop for x from 1 to (expt values-num input-number) do
            (let ((input-entry) (new-values))
                (loop for e in current-values and index from 0 do
                    (push (nth e input-values) input-entry)
                    (if (= 0 (rem x (expt values-num index)))
                        (push (rem (+ 1 e) values-num) new-values)
                        (push e new-values)))
                (setf current-values (reverse new-values))
                (when biasp (push 1 input-entry))
                (push input-entry generated-input)))
    generated-input))

;; Gera uma lista com as respostas esperadas
;;     Parâmetros
;;         inputs: lista com as combinações de entradas
;;         desired-responses: um par (x y) onde:
;;             x: uma combinação de entradas
;;             y: o valor de saída esperadas
;;         default-response: o valor de saída para todas as outras entradas
;;         que não estiverem contidas na lista desired-responses
;;     Retorna
;;         Uma lista onde cada índice contém a saída esperada para a entrada correspondente
;;         àquele índice.
(defun generate-responses (inputs desired-responses default-response)
    (let ((response-list))
        (loop for input in inputs and index from 0 do
            (loop for response in desired-responses do
                (push 
                    (if (equal (first response) input)
                        (second response) 
                        default-response) 
                    response-list)))
    (reverse response-list)))

;; Função que retorna as entradas e repostas esperadas de um arquivo.
;;     Parâmetros
;;         archive-name: nome do arquivo
;;     Retorna
;;         Um par (x y) onde:
;;             x: lista de combinações de entradas
;;             y: lista com as respostas esperadas
(defun load-database (archive-name)
    (let ((archive) (input-responses))
        (setf archive (open archive-name))
        (push (read archive) input-responses)
        (push (read archive) input-responses)
        (close archive-name)
        (reverse input-responses)))

(defun save-database (archive-name inputs desired-responses)
    (let ((archive))
        (setf archive (open archive-name :direction :output :if-exists :supersede))
        (write inputs :stream archive)
        (write desired-responses :stream archive)
        (close archive)))