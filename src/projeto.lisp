;;;; projeto.lisp
;;;; Projeto01 IA - 25/26
;;;; Requer: puzzle.lisp, procura.lisp, problemas.dat

;;; ---------------------------------------------------------
;;; Configuração de caminhos
;;; ---------------------------------------------------------

(defparameter *base-path*
  (make-pathname
   :directory (pathname-directory
               (or *load-truename* *compile-file-truename*))
   :name nil
   :type nil)
  "Diretório base do projeto.")

(setf *default-pathname-defaults* *base-path*)

;;; ---------------------------------------------------------
;;; Função auxiliar para compilar e carregar
;;; ---------------------------------------------------------

(defun compilar-e-carregar (ficheiro)
  "Compila e carrega um ficheiro .lisp."
  (let* ((src (merge-pathnames ficheiro *base-path*))
         (fasl (compile-file src)))
    (load fasl)))

;;; ---------------------------------------------------------
;;; Carregar módulos do projeto
;;; ---------------------------------------------------------

(compilar-e-carregar "puzzle.lisp")
(compilar-e-carregar "procura.lisp")

;;; ---------------------------------------------------------
;;; Ler problemas
;;; ---------------------------------------------------------

(defun ler-problemas (ficheiro)
  "Lê problemas do ficheiro, separados por ###."
  (with-open-file (f ficheiro :direction :input)
    (labels ((trim-tudo (s)
               (string-trim '(#\Space #\Tab #\Newline #\Return) s))
             (push-acc (acc problemas)
               (let ((tacc (trim-tudo acc)))
                 (if (plusp (length tacc))
                     (cons (read-from-string tacc) problemas)
                     problemas))))
      (let ((problemas '())
            (acc ""))
        (loop for linha = (read-line f nil 'eof)
              until (eq linha 'eof) do
                (if (string= linha "###")
                    (progn
                      (setf problemas (push-acc acc problemas))
                      (setf acc ""))
                    (setf acc (concatenate 'string acc linha (string #\Newline)))))
        (setf problemas (push-acc acc problemas))
        (nreverse problemas)))))

;;; ---------------------------------------------------------
;;; Interface / Impressão
;;; ---------------------------------------------------------

(defun imprime-tabuleiro (tab)
  "Imprime o tabuleiro de forma visual no ecrã."
  (format t "~%---------------- Tabuleiro ----------------~%")
  (dolist (linha tab)
    (dolist (c linha)
      (format t "~A "
              (cond
                ((null c) " ")
                ((= c 1) "1")
                ((= c 0) "0"))))
    (format t "~%"))
  (format t "---------------------------------------------~%"))

(defun escrever-tabuleiro-ficheiro (stream tab)
  "Escreve tabuleiro num ficheiro de forma compacta."
  (dolist (linha tab)
    (format stream "  ")
    (dolist (c linha)
      (format stream "~A "
              (cond
                ((null c) " ")
                ((= c 1) "1")
                ((= c 0) "0"))))
    (format stream "~%")))

(defun menu-problema (lista)
  "Apresenta menu de seleção de problema."
  (format t "~%----------- SELECIONE O PROBLEMA -----------~%")
  (loop for i from 1 to (length lista) do
        (format t "~A - Problema ~A~%" i i))
  (format t "Escolha: ")
  (let ((n (read)))
    (values n (nth (1- n) lista))))

(defun menu-algoritmo ()
  "Apresenta menu de seleção de algoritmo."
  (format t "~%----------- SELECIONE O ALGORITMO ----------~%")
  (format t "1 - BFS (Procura em Largura)~%")
  (format t "2 - DFS (Procura em Profundidade)~%")
  (format t "3 - A*  (Procura do Melhor Primeiro)~%")
  (format t "4 - IDA* (Iterative Deepening A*)~%")
  (format t "Escolha: ")
  (case (read)
    (1 'bfs)
    (2 'dfs)
    (3 'a-star)
    (4 'ida-star)
    (t (menu-algoritmo))))

(defun menu-heuristica ()
  "Apresenta menu de seleção de heurística."
  (format t "~%----------- SELECIONE A HEURISTICA ----------~%")
  (format t "1 - h1~%")
  (format t "2 - h2~%")
  (format t "Escolha: ")
  (case (read)
    (1 #'h1)
    (2 #'h2)
    (t (menu-heuristica))))

(defun mostrar-caminho (no-final)
  "Mostra o caminho completo da solução."
  (let ((caminho (construir-caminho no-final)))
    (format t "~%--------- SOLUCAO ENCONTRADA ---------~%")
    (format t "Profundidade da solucao: ~A~%" (no-g no-final))
    (format t "Numero de passos: ~A~%" (length caminho))
    (format t "~%----- Caminho (estado por estado) -----~%")
    (dolist (estado caminho)
      (imprime-tabuleiro estado))
    (format t "~%----------------------------------------~%")
    (values)))

;;; ---------------------------------------------------------
;;; Relatório
;;; ---------------------------------------------------------

(defun limpar-relatorio (ficheiro)
  "Cria novo ficheiro de relatório."
  (with-open-file (f ficheiro :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format f "----- RELATORIO DE DESEMPENHO - PEG SOLITAIRE -----~%")))

(defun escrever-relatorio (ficheiro problema-id algoritmo resultado
                           nos-gerados nos-expandidos ramificacao penetrancia tempo)
  "Escreve resultados completos no ficheiro de relatório."
  (with-open-file (f ficheiro :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format f "~%==============================~%")
    (format f "Problema: ~A~%" problema-id)
    (format f "Algoritmo: ~A~%" algoritmo)
    (format f "~%--- Estatisticas ---~%")
    (format f "Solucao: ~A~%" (if resultado "ENCONTRADA" "NAO ENCONTRADA"))
    (when resultado
      (format f "Profundidade: ~A~%" (no-g resultado)))
    (format f "Nos gerados: ~A~%" nos-gerados)
    (format f "Nos expandidos: ~A~%" nos-expandidos)
    (format f "Fator ramificacao medio: ~,4F~%" ramificacao)
    (format f "Penetrancia: ~,6F~%" penetrancia)
    (format f "Tempo (s): ~,4F~%" tempo)
    
    ;; Escrever solução completa se encontrada
    (when resultado
      (let ((caminho (construir-caminho resultado)))
        (format f "~%--- Solucao (caminho com ~A estados) ---~%" (length caminho))
        (loop for estado in caminho
              for passo from 0
              do (format f "~%Passo ~A:~%" passo)
                 (escrever-tabuleiro-ficheiro f estado))))))

;;; ---------------------------------------------------------
;;; Função principal
;;; ---------------------------------------------------------

(defun iniciar ()
  "Função principal do programa - interface interativa."
  (format t "~%----- PROJETO 01 - PEG SOLITAIRE SOLVER -----~%")
  (format t "~%A carregar problemas...~%")
  (let* ((problemas (ler-problemas (merge-pathnames "problemas.dat" *base-path*)))
         (relatorio (merge-pathnames "resultados.txt" *base-path*)))
    (limpar-relatorio relatorio)
    (format t "~%Total problemas: ~A~%" (length problemas))

    (multiple-value-bind (problema-id problema-escolhido)
        (menu-problema problemas)

      (let ((algoritmo (menu-algoritmo)))
        (format t "~%Problema escolhido:~%")
        (imprime-tabuleiro problema-escolhido)

        (cond
          ((eq algoritmo 'bfs)
           (format t "~%Executando BFS...~%")
           (multiple-value-bind (resultado ng ne ramificacao penetrancia tempo)
               (bfs problema-escolhido #'objetivo? #'gera-sucessores)
             (escrever-relatorio relatorio problema-id "BFS" resultado ng ne ramificacao penetrancia tempo)
             (if resultado
                 (mostrar-caminho resultado)
                 (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%"))))

          ((eq algoritmo 'dfs)
           (format t "~%Qual a profundidade limite? ")
           (let ((lim (read)))
             (format t "~%Executando DFS com limite ~A...~%" lim)
             (multiple-value-bind (resultado ng ne ramificacao penetrancia tempo)
                 (dfs problema-escolhido #'objetivo? #'gera-sucessores lim)
               (escrever-relatorio relatorio problema-id
                                   (format nil "DFS(lim=~A)" lim)
                                   resultado ng ne ramificacao penetrancia tempo)
               (if resultado
                   (mostrar-caminho resultado)
                   (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%")))))

          ((eq algoritmo 'a-star)
           (let ((hfun (menu-heuristica)))
             (format t "~%Executando A* (~A)...~%"
                     (if (eq hfun #'h1) "h1" "h2"))
             (multiple-value-bind (resultado ng ne ramificacao penetrancia tempo)
                 (a-star problema-escolhido #'objetivo? #'gera-sucessores hfun)
               (escrever-relatorio relatorio problema-id
                                   (format nil "A*(~A)" (if (eq hfun #'h1) "h1" "h2"))
                                   resultado ng ne ramificacao penetrancia tempo)
               (if resultado
                   (mostrar-caminho resultado)
                   (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%")))))

          ((eq algoritmo 'ida-star)
           (let ((hfun (menu-heuristica)))
             (format t "~%Executando IDA* (~A)...~%"
                     (if (eq hfun #'h1) "h1" "h2"))
             (multiple-value-bind (resultado ng ne ramificacao penetrancia tempo)
                 (ida-star problema-escolhido #'objetivo? #'gera-sucessores hfun)
               (escrever-relatorio relatorio problema-id
                                   (format nil "IDA*(~A)" (if (eq hfun #'h1) "h1" "h2"))
                                   resultado ng ne ramificacao penetrancia tempo)
               (if resultado
                   (mostrar-caminho resultado)
                   (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%")))))

          (t
           (format t "~%Algoritmo invalido.~%")))))))