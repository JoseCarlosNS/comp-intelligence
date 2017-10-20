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
