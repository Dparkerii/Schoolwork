;;David Parker II
;;Arsenie Jurgenson
;;Cassandra Bailey
;;CS480 Summer 2016
;;Dr. Duric
;;Project

(defun mapcoloring (g colors)
  ;;Author: David Parker II, Arsenie Jurgenson
  "Wrapper function to produce a valid map coloring using the Modified Greedy Algorithm"
  (print "Cutset:")
  (print (mga g))
                                        ;Please run coloring seprately after setting DEFPARAMETER *50-states* and DEFPARAMETER *colors* parameters manually
                                        ;run the coloring as follows:
                                        ; coloring `[cutset output from mapcoloring function]
                                        ; example: (coloring  `(MO CO TN ID PA SD MA VA IN TX WI AZ GA NE KY AR NV))
  )

(defun MGA (g)
  ;;Author: David Parker, Cassandra Bailey
  "Modified Greedy Algorithm to produce a cutset, containing no cycles, of a map"
  (let ((cutset nil) (i 1) (gset '()) (tempF '()) (tempg_i '()))
    ;;initialize weights for g
    (setf g (addweight g))
    ;;add g to gset
    (setf gset (cons g gset))
    ;;removeleaves and start bulk of algorithm
    (setf g (removeleaves g))
    ;;do while tempg is not null
    (do ((tempg g)) ((null tempg) gset)
      ;;getmin and put into cutset
      (setf cutset (cons (getmin tempg) cutset))
      ;;remove vertex from g
      (setf tempg (removevertex (second (getmin tempg)) tempg))
      ;;increment i
      (incf i)
      ;;removeleaves and update weights
      ;;(updateweights tempg)
      (setf tempg (removeleaves tempg))
      (setf gset (cons tempg gset)))
    (remove nil gset)
    (setf cutset (getsecond cutset))
    ;;end phase 1


    ;;begin phase 2
    ;;Author: Cassandra Bailey
    ;;iterate through the cutset, v's to check are in cutset, not G
    (dotimes (i (- (list-length cutset) 1) cutset)
      ;;this loop is to compare the v in the cutset to each v in g_i
      (dolist (G_i Gset cutset)
        (dolist (v_i G_i)
          ;;tempF = F \ {v_i}
          (setf tempF (remove (second v_i) cutset))
          ;;this dolist does newgsubi <= remove (F \ vsubi) from gsubi
          (dolist (v tempF)
            (setf tempG_i (removevertex v G_i))
            (setf tempG_i (removeleaves tempG_i)))
          ;;if v_i is not in tempgG_i then the cycle intersects with F\{v_i}
          ;;so remove v_i froom F by just setting F to tempF

          (if (not (member (second v_i) (getsecond tempG_i)))
              (setf cutset tempf)
              )
          )
        ))))
    ;;;;;;; endloop

;;;;;;;;;;;;;;;;;;PHASE 1 HELPER FUNCTIONS;;;;;;;;;;;;;;;;;

(defun updateweights (vertex g)
  ;;Author: David Parker II
  "Updates weights"
   ;;;1 link
  (let* ((v (getv vertex g)) (temp (getv (first (third v)) g)))
    (if (eq (length (third v)) 1)
        (progn (setf temp (- (first (getv (first (third v)) g)) (getcost vertex g)))
               (setf (first v) (- (first v) (getcost vertex g)))))))

(defun removevertex (vertex g)
  ;;Author: David Parker II
  "Cycles through edges shared by vertex and removes link | (removevertex (vertex g))"
  ;;should remove v from g, along with any edges v is in
  (if (not(null g))
      (let ((deleted (getv vertex g)))
        ;;cycle list of links
        (dolist (i (third deleted))
          ;;remove vertex from i's edge list
          (if (third (getv i g))
              (setedges i (remove (second deleted) (third (getv i g))) g)
              ))
        (setf g (remove deleted g)))))

(defun setedges (vertex edges g)
  ;;Author: David Parker II
  "sets the edges of a vertex when a change occurs"
  (setf (third (getv vertex g)) edges))

(defun removeleaves (g)
  ;;Author: David Parker II
  "Remove leaves aka elements with degree 0 or 1"
  (let ((lst (sortdegree g)))
    (do () ((> (length (third(first lst))) 1) lst)  ;;if smallestdegree is > 1 ret lst. otherwise loop do the following if
      ;;(if (null lst) lst) i dont think this is necessary but leave it commented out for now
      (if (equal (length (third(first lst))) 0)
          ;;if deg 0, [check if lst length 0. if so return. otherwise, remove first vertex from lst]
          (if (equal (list-length lst) 0) (return) (setf lst (rest lst))))
      (if (equal (length (third(first lst))) 1)
          ;;call removevertex if degree 1
          (progn ;;(updateweights (second (first lst)) g)
            (setf lst (removevertex (second (first lst)) lst)))
          (setf lst (sortdegree lst)))
      )))

(defun addweight (g)
  ;;Author: David Parker II
  "Initialize all weights to 1 | (initializeweights graph)"
  (let ((output '()))
    ;;add a weight value to front of each element
    (dolist (i g)
      (setf output (append output (list (cons '1 i))))
      )
    output))

(defun getmin (g)
  ;;Author: David Parker II
  "Get minimum weighted vertex | (getmin graph)"
  (let ((min (first g)))
    ;;cycle list
    (dolist (v G)
      ;;check which weight is smaller
      (if (> (getcost (second min) g) (getcost (second v) g))
          ;;set new min
          (setf min v)))
    ;;return min
    min))

(defun getv (vertex g)
  ;;Author: David Parker II
  "Get V in a graph | (getweight vertex graph)"
  (if (and (eq (second (first g)) vertex) (not (null g)))
      ;;return found vertex
      (first g)
      ;;keep searching
      (if (not (null g))
          (getv vertex (rest g))
          nil)))

(defun getcost (vertex g)
  ;;Author: David Parker II
  "Get C(e) | (getcost vertex graph)"
  (let ((temp (getv vertex g)))
    ;;calculate C(e) <- w(vi)/d(vi)
    (if (= (length (third temp)) 0)
        0
        (/ (car temp) (length (third temp))))))

(defun sortdegree (g)
  ;;Author: David Parker II
  "sorts graph by number of edges"
  (sort g #'< :key (lambda (p) (length (third p)))))

(defun sortweight (g)
  ;;Author: David Parker II
  "sorts graph by weights"
  (sort g #'< :key (lambda (p) (first p))))

(defun getsecond (g)
  ;;Author: David Parker II
  "Gets list of vectors | (getsecond graph)"
  (let ((output nil))
    (dolist (i g)
      (setf output (cons (second i) output)))
    output))

;;[PHASE 3]
(defun coloring (f)
                                        ;Author: Arsenie Jurgenson
  "Find a valid color solution using information from MGA"

  (gen-coloring (form-color-con f (rest *colors*))))
                                        ;(coloring `(IN NV AR GA OK IA UT VA MA KY ID PA SD CO MO TN))

(defun gen-coloring (color-con-lst)
                                        ;Author Arsenie Jurgenson
  "Coloring of the cutset by brute force"
  (let ((stack `((ROOT ())))(coloring nil)(vertex nil)(countryNum 0))
    (print "Looking for coloring solutions for F...")
    (do ()((not stack)());end loop
                                        ;loop: pop vertex off the stack
      (setf vertex (first stack))
      (setf stack (rest stack))

                                        ;remove coloring tuples from previous (invalid) coloring paths when
                                        ;the full coloring has been found
      (if (equalp (first(first(last color-con-lst))) (first (first coloring)))
          (do ()
                                        ;stop looping when the vertex popped off the stack is the same as first in country in the coloring
              ((equalp (first vertex) (first (first coloring)))
                                        ;end loop: remove one last coloring tuple
               (setf coloring (rest coloring))
               (setf countryNum (1- countryNum)))
                                        ;loop: remove coloring tuples until reached the current color being colored
            (setf coloring (rest coloring))
            (setf countryNum (1- countryNum))))

                                        ;add vertex to coloring solution
      (setf coloring (cons vertex coloring))

      (if (and (not (equalp (first(first(last color-con-lst)))(first vertex))) (> (length color-con-lst) countryNum));if haven't reached the end of the list
          (progn
            (dolist (color (reverse (first(rest (first(subseq color-con-lst countryNum (1+ countryNum)))))));loop through vertex colors
                                        ;add each option to the stack
              (setf stack (cons (cons (first (first (subseq color-con-lst countryNum (1+ countryNum)))) (list(list color))) stack)))))

                                        ;increment country number
      (setf countryNum (1+ countryNum))

      (if (> countryNum (length color-con-lst));if have complete solution in the coloring
          (progn
            (if (col-check  ;check if the solution is valid
                 (make-graph (getfirsts (rest(reverse coloring))) *50-states*)
                 (rest(reverse coloring)))
                (let ((color-con nil))
                  (setf coloring (rest (reverse coloring)))

                  (print "Found a valid solution for F:")
                  (print coloring);if found solution

                                        ;add trivial states to the solution (i.e alaska and hawaii)
                  (dolist (tuple *50-states*)
                    (cond((not (first(rest tuple))) ;if a singleton
                          (setf coloring (cons (cons(first tuple) (list(list(first *colors*)))) coloring))
                          (print "Adding a trivial problem to solution:")
                          (print (cons(first tuple) (list(list(first *colors*))))))))

                                        ;reconstrain the graph
                  (do((size-of-coloring 0))
                                        ;stop when the coloring hasn't changed since previous iteration
                     ((equalp size-of-coloring (length coloring))(print "Cannot Constrain further, performing DPS on the remainder"))
                                        ;update size-of-coloring
                    (setf size-of-coloring (length coloring))

                    (print "Color Constraining the graph...")
                    (setf color-con (sort-graph (color-con-gen coloring (make-gprime (getfirsts coloring)))))

                                        ;PUT IF HERE, if first in gprime is empty brake out of the loop
                    (setf coloring nil);reset coloring (will be rebuilt from color con)

                    (dolist (tuple color-con)
                      (cond
                        ((< (length(first(last tuple))) 1);if gprime-color-con has nil values (i.e current coloring is NOT a solution)
                         (print "Color-Con Suggests This is NOT a Solution trying another..."))
                        ((< (length(first(last tuple))) 2); tuple is constrained to just 1 color and should be added to coloring
                         (setf coloring (cons tuple coloring)))))

                    (print "Solution after Constraining:")
                    (setf coloring (print(reverse coloring)))

                    (setf coloring (color-whole coloring)); color the remainder of the graph
                    (if coloring (return-from gen-coloring coloring) ; if found solution return it
                        )))))))))

(defun color-whole (color-con-lst)
                                        ;Author Arsenie Jurgenson
  "Colors remainder of the Set"
  (let ((gprime-edges (make-subgraph (getfirsts (get-degree-one color-con-lst))(addweight *50-states*)))
        (solution (get-degree-one color-con-lst))(new-tuple nil))
                                        ;add trivial states to the solution (i.e KS)
    (dolist (tuple gprime-edges)
      (cond((not (first(rest tuple))) ;if no uncolored neighbors left
                                        ;pick an available color
            (setf new-tuple (cons (first tuple) (list(list(first(first(last(get-tuple (first tuple) color-con-lst))))))))
                                        ;remove from list of uncolored
            (print "Adding a trivial problem to solution:")
            (print new-tuple)
            (if (not(rest new-tuple));if no colors left, fail solution
                (progn (print "No colors left to pick") (return-from color-whole nil)))

                                        ;add to solution
            (setf solution (cons new-tuple solution)))))

                                        ;update color con lst
    (setf color-con-lst (sort-graph (color-con-gen solution (make-gprime (getfirsts solution)))))
                                        ;update gprime-edges
    (setf gprime-edges (make-subgraph (getfirsts (get-degree-one color-con-lst))(addweight *50-states*)))

                                        ; load 1 degree tuples into the solutions
    (setf solution nil)
    (dolist (tuple color-con-lst)
      (cond
        ((< (length(first(last tuple))) 2); tuple is constrained to just 1 color and should be added to coloring
         (setf solution (cons tuple solution)))))
    (print "Solution Updated with degree 1 constraints")
    (print solution)

    (print "Tree Traversal Path:")
    (dolist (node-name (print(stepfunction gprime-edges)))
                                        ;pick first color from constraint and make a coloring tuple new-tuple
      (setf new-tuple (cons node-name (list(list(first(first(last(get-tuple node-name color-con-lst))))))))
      (print "Assigning a color:")
      (print new-tuple)

      (if (not(rest new-tuple));if no colors left, fail solution
          (progn (print "No colors left to pick") (return-from color-whole nil)))

      (setf solution (cons new-tuple solution)); add to solution
                                        ;update color-con-lst
      (setf color-con-lst (sort-graph (color-con-gen solution (make-gprime (getfirsts solution)))))
      (setf gprime-edges (make-subgraph (getfirsts (get-degree-one color-con-lst))(addweight *50-states*)))
      )solution))

                                        ;Authors: Arsenie Jurgenson, David Parker
(defun stepfunction (g)
  "Function steps through graph w/ cutset removed to break problem into smaller sections"
  (let ((result nil) (stack nil) (node nil) (tempnode nil)(temploop nil))
    ;;while length result < length input
    (do ((glength (length g)))
        ((>= (length result) glength));test to quit
                                        ;loop:
                                        ;get first node not in result
      (setf temploop t)
      (dolist (v g)
                                        ;if not in results and havent already added one this iteration
        (if (and temploop (not (in-lst (first v) result)))
            (progn
              (setf stack (cons v stack));push v onto the stack
              (setf temploop nil)))
        (do ()((not stack)());end loop
                                        ;pop node off the stack
          (setf node (first stack))
          (setf stack (rest stack))
          (setf result (cons (first node) result))

          (dolist (neighbor (first(rest node)))
            (setf tempnode (get-tuple neighbor g));save the node
            (setf g (remove tempnode g));remove the old version
            (setf tempnode (cons (first tempnode)  (list(remove (first node) (first(rest tempnode))))))
            (setf stack (cons tempNode stack));push v onto the stack
            ))))(reverse result)))
                                        ;(stepfunction `((AL (FL MS)) (CA (OR)) (CT (RI NY)) (DE (MD NJ)) (DC (MD)) (FL (AL)) (IL (WI)) (KS NIL) (LA (MS)) (ME (NH)) (MD (DE WV DC)) (MI (OH WI)) (MN (WI ND)) (MS (LA AL)) (NH (ME VT)) (NJ (NY DE)) (NY (NJ CT VT)) (NC (SC)) (ND (MN)) (OH (WV MI)) (OR (WA CA)) (RI (CT)) (SC (NC)) (VT (NY NH)) (WA (OR)) (WV (OH MD)) (WI (MN IL MI))))

(defun get-degree-one (graph)
                                        ;Author: Arsenie Jurgenson
  "self explanatory"
  (let ((nodes-degree-one nil))
    (dolist (tuple graph)
      (cond
        ((equalp (length(first(last tuple))) 1); tuple is constrained to just 1 color and should be added to coloring
         (setf nodes-degree-one (cons tuple nodes-degree-one)))))
    (reverse nodes-degree-one)))
                                        ;(getfirsts (get-degree-one `((HI (RED)) (AK (RED)) (IN (GRN)) (NV (GRN)) (AR (BLU)) (GA (GRN)) (OK (YLW)) (IA (BLU)) (UT (BLU)) (VA (GRN)) (MA (GRN)) (KY (BLU)) (ID (YLW)) (PA (GRN)) (SD (GRN)) (CO (GRN)) (MO (GRN)) (TN (YLW)) (WY (RED)) (NM (RED)) (TX (GRN)) (AZ (YLW)) (NE (YLW)) (MT (BLU)) (ND (RED YLW)) (KS (RED BLU)) (CA (RED BLU)) (LA (RED YLW)) (MS (RED GRN)) (AL (RED BLU)) (NC (RED BLU)) (IL (RED YLW)) (MN (RED YLW)) (OH (RED YLW)) (WV (RED YLW)) (OR (RED BLU)) (MD (RED BLU YLW)) (DE (RED BLU YLW)) (NJ (RED BLU YLW)) (NY (RED BLU YLW)) (WA (RED GRN BLU)) (NH (RED BLU YLW)) (VT (RED BLU YLW)) (CT (RED BLU YLW)) (RI (RED BLU YLW)) (DC (RED BLU YLW)) (WI (RED GRN YLW)) (FL (RED BLU YLW)) (SC (RED BLU YLW)) (MI (RED BLU YLW)) (ME (RED GRN BLU YLW)))))

(defun make-subgraph (cutset g);Author: David Parker
  (dolist (v cutset)
    (setf g (removevertex v g)))
  (removefirsts g))

(defun sort-graph (g);Author: David Parker
  "sorts a graph by degree of the second variable in the graph tuple"
  (sort g #'< :key (lambda (p) (length (second p)))))

(defun color-con-gen (f-solution g-prime)
                                        ;Author: Arsenie Jurgenson
  "Generate Gprime_cc: contstrain g-prime to not conflict with f-s solution, format ((A (R G B)) (B (R)) (C(B G)))"
                                        ;look at at f-solution, reduce g-prime color constraints based on neighbors in G_0
  (let ((color-con-gen f-solution))    ;add all f-color-tuples to color-con-gen
                                        ;constrain g-prime
    (dolist (f-color-tuple f-solution)
                                        ;for each tuple in F, get all neighbors
      (dolist (neighbor (first(rest (get-tuple (first f-color-tuple) *50-states*))))
                                        ;for each neighbor of a node in f
        (let ((old-tuple (get-tuple neighbor g-prime))(new-tuple nil))
                                        ;remove the color of F from the neighbor
          (cond (old-tuple ;if the neighbor is in g-prime
                 (setf new-tuple ;remove the color from the tuple and set result to new-tuple
                       (cons (first old-tuple) (cons (remove (first(first (rest f-color-tuple))) (first(rest old-tuple))) ())))
                 (setf g-prime (cons new-tuple (remove old-tuple g-prime)));replace old with new
                 )))))
                                        ;combine f-solution and g-prime color constraints
    (setf color-con-gen (append color-con-gen g-prime))
    (sort-graph color-con-gen)))

(defun form-color-con (countries colors)
                                        ;Author: Arsenie Jurgenson
  "Takes in a list of countries and a list of colors and puts into a color-con
      form, i.e. ((A (RED GRN BLU)) (B (RED GRN BLU)) (C (RED GRN BLU)))"
  (let ((color-con-form nil))
    (dolist (country countries)
                                        ;store tuples in a (B (R)) format instead of (B R) format, i.e. constraints format instead of solution format
      (setf color-con-form (cons (cons country (cons colors ())) color-con-form)))
    (reverse color-con-form)))
                                        ;(form-color-con `(A B C) *colors*)

(defun get-tuple (name graph)
                                        ;Author: Arsenie Jurgenson
  "return a tuple from a unweighted graph"
  (dolist (tuple graph)
    (if (equalp name (first tuple))
        (return-from get-tuple tuple))))

(defun make-gprime (F)
                                        ;Author: Arsenie Jurgenson
  "return Gprime with all possible colors on each node = G\F, format ((A (R G B)) (B (R G B)) (C(R B G)))"
  (let ((gprime nil))
    (dolist (tuple *50-states*);for each in G
      (if (not(in-lst (first tuple) F));if not in f, add to gprime
          (setf gprime (cons (cons (first tuple) (cons *colors* ())) gprime))))
    (reverse gprime)))
                                        ;(make-g-prime `(IN NV AR GA OK IA UT VA MA KY ID PA SD CO MO TN))

(defun make-graph (vertices-lst graph)
                                        ;Author: Arsenie Jurgenson
  "Takes in a list of vertices and returns a subgraph of graph that contains only those verteces"
  (let ((output nil))
    (dolist (tuple graph);for each tuple in the graph
      (if (in-lst (first tuple) vertices-lst);if the touple is in the vertices-lst
          (let ((new-edges nil))
            (dolist (neighbor (first(rest tuple)));for each neighbor
              (if (in-lst neighbor vertices-lst);if neighbor is in v-lst
                  (setf new-edges (cons neighbor new-edges))));save the neighbor (edge)
                                        ;add the node to output
            (setf output (cons (cons (first tuple) (cons (reverse new-edges)())) output)))))
    (reverse output)))
                                        ;(make-graph `(IN NV AR GA OK IA UT VA MA KY ID PA SD CO MO TN) *50-states*)


(defun in-lst (element lst)
                                        ;Author: Arsenie Jurgenson
  "returns true if element is in lst, there is probably a built-in function for this"
  (dolist (iterator lst)
    (if(equalp element iterator)
       (return-from in-lst t)
       )))

(defun removefirsts (input-graph)
                                        ;Author: Arsenie Jurgenson
  "return a graph without the first elment of each tuple, i.e. remove weights"
  (let ((output nil))
    (dolist (tuple input-graph)
      (setf output (cons (rest tuple) output)))
    (reverse output)))

(defun getfirsts (g)
                                        ;Author: Arsenie Jurgenson
  "Returns a list of first elements of tuples of a graph, be it weights or vertices"
  (let ((output nil))
    (dolist (i g)
      (setf output (cons (first i) output)))
    (reverse output)))

(defun get-color (country coloring)
                                        ;Author: Arsenie Jurgenson
  "Returns the color of a particular country from a solution"
  (dolist (col-pair coloring)
    (if (equalp country (first col-pair))
        (return-from get-color (first(rest col-pair))))))

(defun col-check (amap coloring)
                                        ;Author: Arsenie Jurgenson
  "Checks if a solution is valid for a amap graph"
                                        ; for each country-pair in the association map
  (dolist (country-pair amap t);return true if checked all
    (let ((country-color (get-color (first country-pair) coloring)))
                                        ;for each neighbor of the country
      (dolist (neighbor (first (rest country-pair)))
        (if (equalp country-color (get-color neighbor coloring))
            (return-from col-check nil);return nil if same color as neighbor
            )))))
