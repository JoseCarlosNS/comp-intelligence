;; Função que retorna as entradas e repostas esperadas de um arquivo.
;;     Parâmetros
;;         file-name: nome do arquivo
;;     Retorna
;;         O código LISP presente o arquivo
(defun loadf (file-name)
    (let ((file) (data))
        (setf file (open file-name))
        (setf data (read file))
        (close file)
        data))

;; Função que salva código LISP em um arquivo
;;     Parâmetros:
;;         file-name: nome do arquivo.
;;         data: os dados a serem salvos.
(defun savef (file-name data)
    (let ((file))
        (setf file (open file-name :direction :output :if-exists :supersede))
        (write data :stream file)
        (close file)))
