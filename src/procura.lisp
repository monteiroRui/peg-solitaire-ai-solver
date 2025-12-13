;;;; procura.lisp
;;;; Algoritmos de procura: BFS, DFS, A* e IDA*
;;;; Projeto01 IA - 25/26

;;; ---------------------------------------------------------
;;; Estrutura do nó
;;; ---------------------------------------------------------

(defun cria-no (estado &optional (g 0) (pai nil) (h 0))
  "Cria um nó com estado, custo g, pai e heurística h."
  (list estado g pai h))

(defun no-estado (no) (car no))
(defun no-g (no) (second no))
(defun no-pai (no) (third no))
(defun no-h (no) (fourth no))

(defun no-f (no)
  "Calcula f(n) = g(n) + h(n) para A*."
  (+ (no-g no) (no-h no)))

;;; ---------------------------------------------------------
;;; Funções auxiliares comuns
;;; ---------------------------------------------------------

(defun construir-caminho (no-final)
  "Reconstrói o caminho desde a raiz até ao nó final."
  (labels ((auxiliar (n acumulador)
             (if (null n)
                 acumulador
               (auxiliar (no-pai n) (cons (no-estado n) acumulador)))))
    (auxiliar no-final '())))

(defun no-existep (estado lista-nos)
  "Verifica se um estado existe numa lista de nós."
  (cond
    ((null lista-nos) nil)
    ((equalp estado (no-estado (car lista-nos))) t)
    (t (no-existep estado (cdr lista-nos)))))

(defun tempo-segundos (t0 t1)
  "Converte tempo interno para segundos."
  (/ (- t1 t0) (float internal-time-units-per-second)))

(defun calcular-estatisticas (no-solucao nos-gerados nos-expandidos total-sucessores)
  "Calcula estatísticas de desempenho: (ramificacao profundidade penetrancia)."
  (let* ((ramificacao (if (> nos-expandidos 0)
                          (/ total-sucessores (float nos-expandidos))
                        0.0))
         (profundidade (if no-solucao (no-g no-solucao) 0))
         (penetrancia (if (and no-solucao (> nos-gerados 0))
                          (/ profundidade (float nos-gerados))
                        0.0)))
    (values ramificacao profundidade penetrancia)))

(defun finalizar-procura (t0 no-solucao nos-gerados nos-expandidos total-sucessores)
  "Finaliza procura e retorna valores padronizados."
  (let ((tempo (tempo-segundos t0 (get-internal-real-time))))
    (multiple-value-bind (ramificacao profundidade penetrancia)
        (calcular-estatisticas no-solucao nos-gerados nos-expandidos total-sucessores)
      (declare (ignore profundidade))
      (values no-solucao nos-gerados nos-expandidos ramificacao penetrancia tempo))))

;;; ---------------------------------------------------------
;;; BFS (Procura em Largura)
;;; ---------------------------------------------------------

(defun bfs (estado-inicial objetivo-p sucessores-fn)
  "Procura em largura. Explora nível por nível."
  (let ((t0 (get-internal-real-time)))
    (labels
        ((bfs-auxiliar (abertos fechados nos-gerados nos-expandidos total-sucessores)
           (cond
             ((null abertos)
              (finalizar-procura t0 nil nos-gerados nos-expandidos total-sucessores))

             (t
              (let* ((no (car abertos))
                     (resto (cdr abertos))
                     (estado (no-estado no)))

                (cond
                  ((funcall objetivo-p estado)
                   (finalizar-procura t0 no nos-gerados nos-expandidos total-sucessores))

                  ((no-existep estado fechados)
                   (bfs-auxiliar resto fechados nos-gerados nos-expandidos total-sucessores))

                  (t
                   (let* ((sucessores (funcall sucessores-fn estado))
                          (novo-total (+ total-sucessores (length sucessores)))
                          (novos (remove-if
                                  (lambda (e) (no-existep e fechados))
                                  sucessores))
                          (novos-nos (mapcar
                                      (lambda (e) (cria-no e (1+ (no-g no)) no))
                                      novos)))
                     (bfs-auxiliar (append resto novos-nos)
                                   (cons no fechados)
                                   (+ nos-gerados (length novos-nos))
                                   (1+ nos-expandidos)
                                   novo-total)))))))))

      (bfs-auxiliar (list (cria-no estado-inicial)) '() 1 0 0))))

;;; ---------------------------------------------------------
;;; DFS (Procura em Profundidade)
;;; ---------------------------------------------------------

(defun dfs (estado-inicial objetivo-p sucessores-fn profundidade-maxima)
  "Procura em profundidade com limite. Explora ramos até ao limite."
  (let ((t0 (get-internal-real-time)))
    (labels
        ((dfs-auxiliar (abertos fechados nos-gerados nos-expandidos total-sucessores)
           (cond
             ((null abertos)
              (finalizar-procura t0 nil nos-gerados nos-expandidos total-sucessores))

             (t
              (let* ((no (car abertos))
                     (resto (cdr abertos))
                     (estado (no-estado no)))

                (cond
                  ((funcall objetivo-p estado)
                   (finalizar-procura t0 no nos-gerados nos-expandidos total-sucessores))

                  ((>= (no-g no) profundidade-maxima)
                   (dfs-auxiliar resto fechados nos-gerados nos-expandidos total-sucessores))

                  ((no-existep estado fechados)
                   (dfs-auxiliar resto fechados nos-gerados nos-expandidos total-sucessores))

                  (t
                   (let* ((sucessores (funcall sucessores-fn estado))
                          (novo-total (+ total-sucessores (length sucessores)))
                          (novos (remove-if
                                  (lambda (e) (no-existep e fechados))
                                  sucessores))
                          (novos-nos (mapcar
                                      (lambda (e) (cria-no e (1+ (no-g no)) no))
                                      novos)))
                     (dfs-auxiliar (append novos-nos resto)
                                   (cons no fechados)
                                   (+ nos-gerados (length novos-nos))
                                   (1+ nos-expandidos)
                                   novo-total)))))))))

      (dfs-auxiliar (list (cria-no estado-inicial)) '() 1 0 0))))

;;; ---------------------------------------------------------
;;; A* - Funções auxiliares
;;; ---------------------------------------------------------

(defun inserir-ordenado-f (no lista)
  "Insere nó ordenadamente numa lista por f(n) crescente."
  (cond
    ((null lista) (list no))
    ((<= (no-f no) (no-f (car lista))) (cons no lista))
    (t (cons (car lista) (inserir-ordenado-f no (cdr lista))))))

(defun inserir-varios-ordenado-f (nos lista)
  "Insere múltiplos nós ordenadamente por f(n)."
  (if (null nos)
      lista
    (inserir-varios-ordenado-f (cdr nos)
                               (inserir-ordenado-f (car nos) lista))))

(defun fechados-obter (estado fechados)
  "Obtém o melhor custo g para um estado nos fechados."
  (cond
    ((null fechados) nil)
    ((equalp estado (caar fechados)) (cdar fechados))
    (t (fechados-obter estado (cdr fechados)))))

(defun fechados-adicionar (estado g fechados)
  "Atualiza ou adiciona estado com custo g nos fechados."
  (cond
    ((null fechados) (list (cons estado g)))
    ((equalp estado (caar fechados)) (cons (cons estado g) (cdr fechados)))
    (t (cons (car fechados) (fechados-adicionar estado g (cdr fechados))))))

;;; ---------------------------------------------------------
;;; A* (Procura A-Estrela)
;;; ---------------------------------------------------------

(defun a-star (estado-inicial objetivo-p sucessores-fn
               &optional (heuristica-fn #'h1))
  "Procura A*. Usa heurística para guiar a procura."
  (let ((t0 (get-internal-real-time)))
    (labels
        ((a-auxiliar (abertos fechados nos-gerados nos-expandidos total-sucessores)
           (cond
             ((null abertos)
              (finalizar-procura t0 nil nos-gerados nos-expandidos total-sucessores))

             (t
              (let* ((no (car abertos))
                     (resto (cdr abertos))
                     (estado (no-estado no))
                     (g (no-g no))
                     (g-anterior (fechados-obter estado fechados)))

                (cond
                  ((funcall objetivo-p estado)
                   (finalizar-procura t0 no nos-gerados nos-expandidos total-sucessores))

                  ((and g-anterior (<= g-anterior g))
                   (a-auxiliar resto fechados nos-gerados nos-expandidos total-sucessores))

                  (t
                   (let* ((fechados-novo (fechados-adicionar estado g fechados))
                          (sucessores (funcall sucessores-fn estado))
                          (novo-total (+ total-sucessores (length sucessores)))
                          (novos-nos (mapcar
                                      (lambda (e)
                                        (cria-no e (1+ g) no
                                                (funcall heuristica-fn e)))
                                      sucessores))
                          (abertos-novo (inserir-varios-ordenado-f novos-nos resto)))
                     (a-auxiliar abertos-novo fechados-novo
                                 (+ nos-gerados (length novos-nos))
                                 (1+ nos-expandidos)
                                 novo-total)))))))))

      (a-auxiliar (list (cria-no estado-inicial 0 nil
                                 (funcall heuristica-fn estado-inicial)))
                  '() 1 0 0))))

;;; ---------------------------------------------------------
;;; IDA* - Funções auxiliares
;;; ---------------------------------------------------------

(defun inserir-ordenado-h (estado lista heuristica-fn)
  "Insere estado ordenadamente por h(n)."
  (cond
    ((null lista) (list estado))
    ((<= (funcall heuristica-fn estado)
         (funcall heuristica-fn (car lista)))
     (cons estado lista))
    (t (cons (car lista) (inserir-ordenado-h estado (cdr lista) heuristica-fn)))))

(defun ordenar-sucessores (estados heuristica-fn)
  "Ordena lista de estados por h(n) crescente."
  (labels ((auxiliar (xs acumulador)
             (if (null xs)
                 acumulador
               (auxiliar (cdr xs) (inserir-ordenado-h (car xs) acumulador heuristica-fn)))))
    (auxiliar estados '())))

;;; ---------------------------------------------------------
;;; IDA* (Procura A-Estrela com Aprofundamento Iterativo)
;;; ---------------------------------------------------------

(defun ida-star (estado-inicial objetivo-p sucessores-fn
                 &optional (heuristica-fn #'h1))
  "Procura IDA*. Aprofundamento iterativo com heurística."
  (let ((t0 (get-internal-real-time))
        (infinito most-positive-fixnum))
    (labels
        ((procurar (no limite caminho)
           (let* ((estado (no-estado no))
                  (g (no-g no))
                  (h (no-h no))
                  (f (+ g h)))
             (cond
               ((> f limite) (values nil f 0 0 0))
               ((funcall objetivo-p estado) (values no nil 0 0 0))
               (t
                (let ((sucessores (ordenar-sucessores
                                   (funcall sucessores-fn estado)
                                   heuristica-fn)))
                  (labels
                      ((explorar (xs minimo nos-gerados nos-expandidos total-sucessores)
                         (if (null xs)
                             (values nil minimo nos-gerados nos-expandidos total-sucessores)
                           (let ((e (car xs)))
                             (if (member e caminho :test #'equalp)
                                 (explorar (cdr xs) minimo nos-gerados nos-expandidos total-sucessores)
                               (let ((filho (cria-no e (1+ g) no
                                                     (funcall heuristica-fn e))))
                                 (multiple-value-bind (solucao proximo ng ne ts)
                                     (procurar filho limite (cons e caminho))
                                   (if solucao
                                       (values solucao nil
                                               (+ nos-gerados 1 ng) (+ nos-expandidos ne) (+ total-sucessores ts))
                                     (explorar (cdr xs)
                                               (if (and proximo (< proximo minimo)) proximo minimo)
                                               (+ nos-gerados 1 ng) (+ nos-expandidos ne) (+ total-sucessores ts))))))))))
                    (multiple-value-bind (solucao proximo ng ne ts)
                        (explorar sucessores infinito 0 0 (length sucessores))
                      (values solucao proximo ng (+ ne 1) ts))))))))

         (iterar (limite)
           (let ((no-inicial (cria-no estado-inicial 0 nil
                                      (funcall heuristica-fn estado-inicial))))
             (multiple-value-bind (solucao proximo ng ne ts)
                 (procurar no-inicial limite (list estado-inicial))
               (cond
                 (solucao (values solucao nil ng ne ts))
                 ((or (null proximo) (>= proximo infinito)) (values nil nil ng ne ts))
                 (t (iterar proximo)))))))

      (multiple-value-bind (no-solucao proximo ng ne ts)
          (iterar (funcall heuristica-fn estado-inicial))
        (declare (ignore proximo))
        (finalizar-procura t0 no-solucao (+ 1 ng) ne ts)))))