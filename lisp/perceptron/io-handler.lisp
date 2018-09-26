(require "asdf")
(asdf:load-system :cl-csv)

;; Função que lê um arquivo csv.
;;     Parâmetros
;;         file-name: nome do arquivo
;;             OBS: Espera-se que o arquivo csv siga o modelo
;;                 1-(tags)
;;                 2-(entrada 1)
;;                 3-(entrada 2)
;;                 4-...
;;             Onde cada entrada consiste em uma lista de valores, e seu último valor 
;;             é o valor esperado da entrada
;;     Retorna
;;         Um trio (x y z) onde:
;;             x: as "tags" ou nomes das entradas
;;             y: entradas do perceptron
;;             z: saídas desejadas
(defun load-csv (file-name)
    (let ((data) (tags) (input) (responses))
        (setf data (cl-csv:read-csv file-name))
        (setf tags (subseq (first data) 0 (- (list-length (first data)) 1)))
        (setf input (mapcar (lambda (x) (mapcar #'parse-integer x)) (rest data)))
        (setf responses (mapcar #'first (mapcar #'last input)))
        (setf input (mapcar (lambda (x) (subseq x 0 (- (list-length x) 1))) input))
        (list tags input responses)))

;; Função que salva os dados dos pesos de um perceptron em um arquivo
;;     Parâmetros:
;;         file-name: nome do arquivo.
;;         perceptron: o perceptron cujos pesos serão salvos
(defun save-csv (file-name perceptron)
    (let ((data) (file))
        (setf file (open file-name :direction :output :if-exists :supersede))
        (push (perceptron-weights perceptron) data)
        (push (perceptron-inputs perceptron) data)
        (cl-csv:write-csv data :stream file)
        (close file)))
