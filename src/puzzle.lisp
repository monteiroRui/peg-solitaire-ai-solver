;;;; puzzle.lisp
;;;; Lógica específica do Peg Solitaire
;;;; Projeto01 IA - 25/26
;;;; Implementação funcional (sem efeitos laterais)

;;; ---------------------------------------------------------
;;; Representação do tabuleiro
;;; ---------------------------------------------------------

(defun tabuleiro-inicial ()
  "Retorna o tabuleiro inicial padrão do Peg Solitaire (32 peças)."
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (1 1 1 1 1 1 1)
    (1 1 1 0 1 1 1)
    (1 1 1 1 1 1 1)
    (nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)))

;;; ---------------------------------------------------------
;;; Acesso ao tabuleiro
;;; ---------------------------------------------------------

(defun linha (i tabuleiro)
  "Obtém a linha do tabuleiro (índice de 1 a 7)."
  (nth (1- i) tabuleiro))

(defun coluna (j tabuleiro)
  "Obtém a coluna do tabuleiro (índice de 1 a 7)."
  (mapcar (lambda (l) (nth (1- j) l)) tabuleiro))

(defun celula (i j tabuleiro)
  "Obtém o valor de uma célula do tabuleiro."
  (let ((l (linha i tabuleiro)))
    (when l (nth (1- j) l))))

(defun celula-validap (i j tabuleiro)
  "Verifica se a célula é válida (contém 0 ou 1, não nil)."
  (let ((v (celula i j tabuleiro)))
    (or (eql v 0) (eql v 1))))

;;; ---------------------------------------------------------
;;; Atualização funcional do tabuleiro
;;; ---------------------------------------------------------

(defun substituir-posicao (n lista novo-valor)
  "Substitui o elemento na posição n por novo-valor."
  (cond
    ((null lista) '())
    ((= n 1) (cons novo-valor (cdr lista)))
    (t (cons (car lista)
             (substituir-posicao (1- n) (cdr lista) novo-valor)))))

(defun substituir (i j tabuleiro novo-valor)
  "Retorna novo tabuleiro com célula (i,j) substituída."
  (cond
    ((null tabuleiro) '())
    ((= i 1)
     (cons (substituir-posicao j (car tabuleiro) novo-valor)
           (cdr tabuleiro)))
    (t (cons (car tabuleiro)
             (substituir (1- i) j (cdr tabuleiro) novo-valor)))))

;;; ---------------------------------------------------------
;;; Validação de movimentos
;;; ---------------------------------------------------------

(defun posicao-validap (i j)
  "Verifica se posição está dentro dos limites do tabuleiro."
  (and (>= i 1) (<= i 7)
       (>= j 1) (<= j 7)))

(defun movimento-validop (origem-i origem-j meio-i meio-j destino-i destino-j tab)
  "Verifica se um movimento é válido: origem com peça, meio com peça, destino vazio."
  (and (posicao-validap origem-i origem-j)
       (posicao-validap meio-i meio-j)
       (posicao-validap destino-i destino-j)
       (celula-validap origem-i origem-j tab)
       (celula-validap meio-i meio-j tab)
       (celula-validap destino-i destino-j tab)
       (eql (celula origem-i origem-j tab) 1)
       (eql (celula meio-i meio-j tab) 1)
       (eql (celula destino-i destino-j tab) 0)))

(defun executar-movimento (origem-i origem-j meio-i meio-j destino-i destino-j tab)
  "Executa um movimento: remove origem e meio, adiciona no destino."
  (substituir destino-i destino-j
              (substituir meio-i meio-j
                          (substituir origem-i origem-j tab 0)
                          0)
              1))

;;; ---------------------------------------------------------
;;; Operadores de movimento
;;; ---------------------------------------------------------

(defun operador-cd (i j tab)
  "Operador Captura Direita: move peça para a direita (i,j) => (i,j+2)."
  (when (movimento-validop i j i (+ j 1) i (+ j 2) tab)
    (executar-movimento i j i (+ j 1) i (+ j 2) tab)))

(defun operador-ce (i j tab)
  "Operador Captura Esquerda: move peça para a esquerda (i,j) => (i,j-2)."
  (when (movimento-validop i j i (- j 1) i (- j 2) tab)
    (executar-movimento i j i (- j 1) i (- j 2) tab)))

(defun operador-cc (i j tab)
  "Operador Captura Cima: move peça para cima (i,j) => (i-2,j)."
  (when (movimento-validop i j (- i 1) j (- i 2) j tab)
    (executar-movimento i j (- i 1) j (- i 2) j tab)))

(defun operador-cb (i j tab)
  "Operador Captura Baixo: move peça para baixo (i,j) => (i+2,j)."
  (when (movimento-validop i j (+ i 1) j (+ i 2) j tab)
    (executar-movimento i j (+ i 1) j (+ i 2) j tab)))

;;; ---------------------------------------------------------
;;; Objetivo e contagem
;;; ---------------------------------------------------------

(defun contar-pecas (tabuleiro)
  "Conta o número total de peças (1s) no tabuleiro."
  (reduce #'+ (mapcar (lambda (l) (count 1 l)) tabuleiro)))

(defun objetivo? (tabuleiro)
  "Verifica se o objetivo foi alcançado (exatamente 1 peça)."
  (= (contar-pecas tabuleiro) 1))

;;; ---------------------------------------------------------
;;; Geração de sucessores
;;; ---------------------------------------------------------

(defun aplicar-operadores (i j tab)
  "Aplica todos os operadores possíveis a partir de (i,j)."
  (remove nil
          (list (operador-cd i j tab)
                (operador-ce i j tab)
                (operador-cc i j tab)
                (operador-cb i j tab))))

(defun gera-sucessores-aux (i j tab)
  "Gera sucessores percorrendo todas as posições do tabuleiro."
  (cond
    ((> i 7) '())
    ((> j 7) (gera-sucessores-aux (1+ i) 1 tab))
    ((eql (celula i j tab) 1)
     (append (aplicar-operadores i j tab)
             (gera-sucessores-aux i (1+ j) tab)))
    (t (gera-sucessores-aux i (1+ j) tab))))

(defun gera-sucessores (tabuleiro)
  "Gera todos os sucessores possíveis de um tabuleiro."
  (gera-sucessores-aux 1 1 tabuleiro))

;;; ---------------------------------------------------------
;;; Heurísticas
;;; ---------------------------------------------------------

(defun peca-movivel? (i j tab)
  "Verifica se a peça em (i,j) pode mover-se."
  (and (eql (celula i j tab) 1)
       (or (operador-cd i j tab)
           (operador-ce i j tab)
           (operador-cc i j tab)
           (operador-cb i j tab))))

(defun contar-pecas-moviveis-aux (i j tab)
  "Conta recursivamente o número de peças que podem mover-se."
  (cond
    ((> i 7) 0)
    ((> j 7) (contar-pecas-moviveis-aux (1+ i) 1 tab))
    ((peca-movivel? i j tab)
     (1+ (contar-pecas-moviveis-aux i (1+ j) tab)))
    (t (contar-pecas-moviveis-aux i (1+ j) tab))))

(defun contar-pecas-moviveis (tab)
  "Retorna o número total de peças que podem mover-se."
  (contar-pecas-moviveis-aux 1 1 tab))

(defun h1 (tab)
  "Heurística base: h(x) = 1 / (peças_móveis + 1).
   Favorece estados com mais peças capazes de se moverem."
  (/ 1.0 (+ 1 (contar-pecas-moviveis tab))))

(defun h2 (tab)
  "Heurística melhorada: h(x) = número_de_peças - 1.
   
   Admissível porque cada movimento remove exatamente 1 peça,
   logo são necessários no mínimo (n-1) movimentos para ir
   de n peças para 1 peça.
   
   Mais informada que h1, especialmente para problemas grandes."
  (float (1- (contar-pecas tab))))