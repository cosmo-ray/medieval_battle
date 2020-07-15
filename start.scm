;           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;                   Version 2, December 2004
;
; Copyright (C) 2020 Matthias Gatto <uso.cosmo.ray@gmail.com>
;
; Everyone is permitted to copy and distribute verbatim or modified
; copies of this license document, and changing it is allowed as long
; as the name is changed.
;
;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;
;  0. You just DO WHAT THE FUCK YOU WANT TO.

(begin

  (define medba_map_w 15)
  (define medba_map_h 20)

  (define medba_map_choose_unit_state 0)
  (define medba_map_move_state 1)
  (define medba_map_atk_state 2)
  (define medba_map_enemy_turn 3)

  (define medba_action
    (lambda (mb eves)
      (letrec(
	      (fwid (ywCntWidgetFather mb))
	      (mbm (ywCntGetEntry (ywCntWidgetFather mb) 1))
	      (cur_pl (lambda () (yeGetIntAt fwid "cur_player")))
	      (flip_pl (lambda (p) (if (= p 0) 0 1)))
	      (tar_pl (lambda () (flip_pl (cur_pl))))
	      (cur_u_idx (lambda () (yeGetIntAt fwid "cur_unit")))
	      (t_u_mrk (lambda () (if (= (cur_pl) 0) "p1u" "p0u")))
	      (t_p (lambda (t) (yeGet t "pos")))
	      (c_t_p (lambda () (t_p (c_t))))
	      (cp_str (lambda () (if (= (cur_pl) 0) "p0" "p1") ))
	      (c_u_mrk (lambda () (if (= (cur_pl) 0) "p0u" "p1u")))
	      (t_units (lambda () (if (= (cur_pl) 0)
				      (yeGet fwid "p1")
				      (yeGet fwid "p0"))))
	      (c_t_idx (lambda () (yeGetIntAt fwid "cur_target")))
	      (c_t_by_idx (lambda (i) (yeGet (t_units) i)))
	      (c_t (lambda () (yeGet (t_units) (c_t_idx))))
	      (c_units (lambda () (if (= (cur_pl) 0)
				      (yeGet fwid "p0")
				      (yeGet fwid "p1"))))
	      (m_state (lambda () (yeGetIntAt fwid "map_state")))
	      (c_u (lambda () (yeGet (c_units) (cur_u_idx))))
	      (c_u_p (lambda () (yeGet (c_u) "pos")))
	      (c_u_p2 (lambda (u) (yeGet u "pos")))
	      (c_u_px (lambda (u) (ywPosX (c_u_p2 u))))
	      (c_u_py (lambda (u) (ywPosY (c_u_p2 u))))
	      (c_c_p (lambda () (yeGet mb "c_c_p")))
	      (mv_trace (lambda () (yeGet mb "mv_trace")))
	      (targets (lambda () (yeGet mb "targets")))
	      (range (lambda () (yeGetIntAt (c_u) "range") ))

	      (to_mv_state
	       (lambda ()
		 (yeSetIntAt fwid "map_state" medba_map_move_state)
		 (yeReCreateArray mb "mv_trace")
		 (yeCreateCopy (c_u_p) (mv_trace))
		 (ywPosSet (c_c_p) (c_u_p))
		 ))

	      (mk_t_at_
	       (lambda (x y p)
		 (yePushBack (mv_trace) p)
		 (ywMapPushNbr mb 1 p "mv-trace"))
		 )
	      (mk_t_at
	       (lambda (x y)
		 (mk_t_at_ x y (ywPosCreate x y))
		 ))
	      (mk_archer_trace_mx
	       (lambda (x y)
		 (if (> x 0) (mk_archer_trace_mx (- x 1) y))
		 (mk_t_at x y)
		 ))
	      (mk_archer_trace_my
	       (lambda (x y)
		 (if (> y 0) (mk_archer_trace_my x (- y 1)))
		 (mk_t_at x y)
		 ))
	      (mk_archer_trace_py
	       (lambda (x y)
		 (if (< (+ y 1) medba_map_h) (mk_archer_trace_py x (+ y 1)))
		 (mk_t_at x y)
		 ))
	      (mk_archer_trace_px
	       (lambda (x y)
		 (if (< (+ x 1) medba_map_w) (mk_archer_trace_px (+ x 1) y))
		 (mk_t_at x y)
		 ))
	      (mk_archer_targets
	       (lambda ()
		 (yeForeach (yeGet fwid (cp_str))
			    (lambda (el a)
			      (if (or (= (ywPosX (car a)) (c_u_px el) )
				      (= (ywPosY (car a)) (c_u_py el) ))
				  (yePushBack (cdr a) el))
			      a
			      )
			    (cons (c_u_p) (targets)))
		 (mk_archer_trace_px (+ (ywPosX (c_u_p)) 1) (ywPosY (c_u_p)))
		 (mk_archer_trace_mx (- (ywPosX (c_u_p)) 1) (ywPosY (c_u_p)))
		 (mk_archer_trace_py (ywPosX (c_u_p)) (+ (ywPosY (c_u_p)) 1))
		 (mk_archer_trace_my (ywPosX (c_u_p)) (- (ywPosY (c_u_p)) 1))
		 ))
	      (mk_other_trace_x
	       (lambda (x y ex)
		 (unless (= x ex) (mk_other_trace_x (+ x 1) y ex))
		 (unless (ywPosIsSame (c_u_p) x y) (mk_t_at x y))
	       ))
	      (mk_other_trace_y
	       (lambda (x y ex ey)
		 (unless (= y ey) (mk_other_trace_y x (+ y 1) ex ey))
		 (mk_other_trace_x x y ex)
	       ))
	      (mk_other_targets
	       (lambda ()
		 (mk_other_trace_y (- (ywPosX (c_u_p)) (range))
				   (- (ywPosY (c_u_p)) (range))
				   (+ (ywPosX (c_u_p)) (range))
				   (+ (ywPosY (c_u_p)) (range)))
		 ))
	      (find_target_in_traces
	       (lambda (t)
		 (yeForeach (mv_trace)
			    (lambda (el a)
			      (if a #t (ywPosIsSame el (t_p t))) ) #f)
		 ))
	      (find_target_
	       (lambda (idx)
		 (cond
		  ((= idx (yeLen (t_units))) ())
		  ((find_target_in_traces (c_t_by_idx idx))
		   (yeSetIntAt fwid "cur_target" idx))
		  (else (find_target_ (+ idx 1)))
		  )
		 ))
	      (find_target (lambda () (find_target_ (c_t_idx)) ))
	      (to_atk_state
	       (lambda ()
		 (ywMapRemoveByStr mb (c_u_p) "cursor")
		 (yeReCreateArray mb "mv_trace")
		 (yeReCreateArray mb "targets")
		 (yeSetIntAt fwid "map_state" medba_map_atk_state)
		 (cond
		  ((< (range) 0) (mk_archer_targets))
		  (else (mk_other_targets))
		  )
		 (find_target)
		 ))

 	      (choose_guy_next_state
	       (lambda ()
		 (if (= (yeGetIntAt mb "action_type") 0)
		     (to_mv_state) (to_atk_state))
		 ))
	      (map_out
	       (lambda ()
		 (begin
		   (yeSetIntAt fwid "current" 1)
		   (yeSetIntAt fwid "map_state" medba_map_choose_unit_state)
		   (yeSetStringAt mbm "pre-text" "")
		   )))

	      (mvp (lambda (u) (yeGetIntAt u "mv")))
	      (have_atk (lambda (u) (yeGetIntAt u "have_attack")))

 	      (chk_mv_x
	       (lambda () (if (yevIsKeyDown eves Y_LEFT_KEY) -1
			      (if (yevIsKeyDown eves Y_RIGHT_KEY) 1 0) )))
 	      (chk_mv_y
	       (lambda () (if (yevIsKeyDown eves Y_UP_KEY) -1
			      (if (yevIsKeyDown eves Y_DOWN_KEY) 1 0) )))


	      (clr_mv_t
	       (lambda ()
		 (yeForeach (mv_trace)
			    (lambda (el a)
			      (ywMapRemoveByStr mb el "mv-trace")
			      ))
		 ))
	      (mv_guy_next_out
	       (lambda ()
		 (yeSetIntAt fwid "cur_target" -1)
		 (clr_mv_t)
		 (yeClearArray (mv_trace))
		 (map_out)))

	      (mv_u
	       (lambda (to)
		 (clr_mv_t)
		 (yeForeach (mv_trace)
			    (lambda (el a)
			      (ywMapMoveByEntity mb (c_u_p) el (c_u))
			      (ywPosSet (c_u_p) el)
			      (ywidRendMainWid)
			      (yuiUsleep 200000)
			      ))
		 (ywMapMoveByEntity mb (c_u_p) to (c_u))
		 (ywPosSet (c_u_p) to)
		 (if (not (= (mvp (c_u)) 0))
		     (yeAddAt (c_u) "mv" (- (- (yeLen (mv_trace)) 1))))
		 ))

	      (can_atk (lambda () (> (c_t_idx) -1)))
	      (rm_target
	       (lambda ()
		 (yeSetStringAt mbm "pre-text" "DEAD\n")
		 (ywMapRemoveByStr mb (c_t_p) (t_u_mrk))
		 (yeRemoveChildByEntity (t_units) (c_t))
		 )
	       )
	      (do_atk_
	       (lambda ()
		 (yeSetIntAt (c_u) "have_attack" 1)
		 (yeSetStringAt mbm "pre-text" "BOOM\n")
		 (yeStringAdd (yeGet mbm "pre-text") "do dmg: ")
		 (yeStringAddInt (yeGet mbm "pre-text") (yeGetIntAt (c_u) "atk"))
		 (yeStringAdd (yeGet mbm "pre-text") "\nHP left: ")
		 (yeAddAt (c_t) "hp" (- (yeGetIntAt (c_u) "atk")))
		 (yeStringAddInt (yeGet mbm "pre-text") (yeGetIntAt (c_t) "hp"))
		 (ywidRendMainWid)
		 (yuiUsleep 1000000)
		 (mk_u_nfo (yeGet mbm "pre-text") (c_t))
		 (ywidRendMainWid)
		 (if (< (yeGetIntAt (c_t) "hp") 1) (rm_target))
		 (yuiUsleep 600000)
		 (ywidRendMainWid)
		 ))
	      (do_atk (lambda () (do_atk_) (mv_guy_next_out) ) )
	      (atk_guy
	       (lambda ()
		 (letrec ()
		   (begin
		     (if (not (can_atk))
			 (yeSetStringAt mbm "pre-text" "No Valide Target")
			 (begin (mk_u_nfo (yeGet mbm "pre-text") (c_t))
				(ywMapRemoveByStr mb (c_t_p) "cursor")))
		     (cond ((yevIsKeyDown eves Y_ESC_KEY) (mv_guy_next_out))
			   ((and (can_atk) (yevIsKeyDown eves Y_ENTER_KEY))
			    (do_atk))
			   ((can_atk) (ywMapPushNbr mb 2 (c_t_p) "cursor"))))
		   )
		 )
	       )
	      (mv_guy
	       (lambda ()
		 (letrec
		     (
		      (mv_guy_enter
		       (lambda ()
			 (mv_u (c_c_p))
			 (mv_guy_next_out)))
		      (l_trc (lambda () (yeLast (mv_trace))))
		      (restore_p
		       (lambda (xy)
			 (begin
			   (ywPosSet (c_c_p) (car xy) (cdr xy))
			   (ywMapRemoveByStr mb (c_c_p) "mv-trace")
			   (yePopBack (mv_trace))
			   (mk_u_nfo (yeGet mbm "pre-text") (c_u))
			   )))
		      (mv_p
		       (lambda (xy)
			 (begin
			   (ywMapPushElem mb (yeCreateInt 2) (c_c_p) "mv-trace")
			   (yeCreateCopy (c_c_p) (mv_trace))
			   (mk_u_nfo (yeGet mbm "pre-text") (c_u))
			   (ywPosSet (c_c_p) (car xy) (cdr xy))
			   )))
		      (chk_case
		       (lambda (xy)
			 (if (ywMapContainEnt mb (car xy) (cdr xy) (c_u)) 0
			     (if (ywMapContainStr mb (car xy) (cdr xy) (c_u_mrk)) 1
				 (if (ywMapContainStr mb (car xy) (cdr xy)
						      (t_u_mrk)) 2 0)))
			 )
		       )
		      (chk_can_mv
		       (lambda (xy)
			 (if (= (chk_case xy) 0)
			     (if (ywPosIsSame (l_trc)
					      (car xy) (cdr xy))
				 (restore_p xy)
				 (if (< (yeLen (mv_trace)) (+ (mvp (c_u)) 1))
				     (mv_p xy))))
			 )
		       )
		      (chk_mv
		       (lambda (x y) (if (or (not (= x 0)) (not (= y 0)))
					 (chk_can_mv
					  (cons (+ (ywPosX (c_c_p)) x)
						(+ (ywPosY (c_c_p)) y))
					  )))
		       )
		      )
		   (begin
		     (ywMapRemoveByStr mb (c_c_p) "cursor")
		     (chk_mv (chk_mv_x) (chk_mv_y))
		     (if (yevIsKeyDown eves Y_ESC_KEY)
			 (mv_guy_next_out)
			 (if (yevIsKeyDown eves Y_ENTER_KEY) (mv_guy_enter)
			     (ywMapPushElem mb (yeCreateInt 1) (c_c_p) "cursor"))
			 )
		     ))))
	      (mk_u_nfo
	       (lambda (str u)
		 (begin
		   (if (= (m_state) medba_map_atk_state)
		       (yeSetString str "Enemy Stats:\n")
		       (yeSetString str "Unit Stats:\n"))
		   (yeStringAdd str "type:")
		   (yeAddEnt str (yeGet u "type"))
		   (yeStringAdd str "\nHP:")
		   (yeStringAddInt str (yeGetIntAt u "hp"))
		   (yeStringAdd str "\nMove Point:")
		   (if (= (m_state) medba_map_atk_state)
		       (yeStringAddInt str (mvp u))
		       (yeStringAddInt str (- (+ (mvp u) 1)
					      (if (= (yeLen (mv_trace)) 0) 1
						  (yeLen (mv_trace)))))
		       )
		   (if (= (have_atk u) 0)
		       (yeStringAdd str "\nCan Attack")
		       (yeStringAdd str "\nAlerady Attack"))
		   (yeStringAdd str "\n-------------")
		   )
		 ))
	      (choose_guy (lambda ()
		(begin
		  (ywMapRemoveByStr mb (c_u_p) "cursor")

		  (if (yevIsKeyDown eves Y_LEFT_KEY)
		      (yeAddAt fwid "cur_unit" (- 1)))
		  (if (yevIsKeyDown eves Y_RIGHT_KEY)
		      (yeIncrAt fwid "cur_unit" ))
		  (mk_u_nfo (yeGet mbm "pre-text") (c_u))
		  (if (yevIsKeyDown eves Y_ESC_KEY)
		      (map_out)
		      (ywMapPushElem mb (yeCreateInt 2) (c_u_p) "cursor")
		      )
		  (if (and (yevIsKeyDown eves Y_ENTER_KEY) (= (have_atk (c_u)) 0))
		      (choose_guy_next_state))
		  )
		))

	      (archer_can_atk
	       (lambda (p)
		 (yeForeach (t_units)
			    (lambda (el a)
			      (if a #t (or (= (ywPosX p) (c_u_px el) )
					  (= (ywPosY p) (c_u_py el) )))
			      ) #f)
		 ))
	      (find_t
	       (lambda ()
		 (yeForeach (t_units)
			    (lambda (el a)
			      (if (< (ywPosTotCases (c_u_p) (t_p el))
				     (ywPosTotCases (c_u_p) a))
				  (t_p el) a)
			      )
			    (ywPosCreate 100 100))
		 )
	       )


	      (add_push_mv_table (lambda(p x y)
				   (yeCreateCopy p (mv_trace))
				   (ywPosAddXY p x y)
				  ))
	      (archer_do_mv
	       (lambda (t ml mp)
		 (cond
		  ( (< (ywPosX t) (ywPosX mp)) (add_push_mv_table mp -1 0) )
		  ( (< (ywPosY t) (ywPosY mp)) (add_push_mv_table mp  0 -1) )
		  ( (> (ywPosX t) (ywPosX mp)) (add_push_mv_table mp 1 0) )
		  ( (> (ywPosY t) (ywPosY mp)) (add_push_mv_table mp 0 1) )
		  )
		 (if (or (= ml 0) (archer_can_atk mp))
		     mp (archer_do_mv t (- ml 1) mp))
		 ))

	      (archer_mk_mv_trace
	       (lambda (t)
		 (yeCreateCopy (c_u_p) (mv_trace))
		 (archer_do_mv t 5 (yeCreateCopy (c_u_p)))
		 )
	       )

	      (archer_auto_mv
	       (lambda ()
		 (unless (archer_can_atk (c_u_p))
		   (mv_u (archer_mk_mv_trace (find_t))))
		 ))

	      (other_do_mv
	       (lambda (t ml mp)
		 (cond
		  ( (< (ywPosX t) (ywPosX mp)) (add_push_mv_table mp -1 0) )
		  ( (< (ywPosY t) (ywPosY mp)) (add_push_mv_table mp  0 -1) )
		  ( (> (ywPosX t) (ywPosX mp)) (add_push_mv_table mp 1 0) )
		  ( (> (ywPosY t) (ywPosY mp)) (add_push_mv_table mp 0 1) )
		  )
		 (if (or (= ml 0) (other_can_atk mp (yeCreateCopy (mv_trace))
						 (yeCreateCopy (c_u_p))))
		     mp (other_do_mv t (- ml 1) mp))
		 ))

	      (other_mk_mv_trace
	       (lambda (t)
		 (yeCreateCopy (c_u_p) (mv_trace))
		 (other_do_mv t 5 (yeCreateCopy (c_u_p)))
		 )
	       )

	      (other_can_atk
	       (lambda (p cpt cpp)
		 (yeSetIntAt fwid "cur_target" -1)
		 (yeClearArray (mv_trace))
		 (yeCopy p (c_u_p))
		 (mk_other_targets)
		 (find_target)
		 (clr_mv_t)
		 (yeCopy cpt (mv_trace))
		 (yeCopy cpp (c_u_p))
		 (can_atk)
		 ))


	      (other_auto_mv
	       (lambda ()
		 (unless (other_can_atk (c_u_p) (yeCreateCopy (mv_trace))
					(yeCreateCopy (c_u_p)))
		   (mv_u (other_mk_mv_trace (find_t))))
		 ))

	      (soldier_of_ai
	       (lambda ()
		 (cond
		  ((< (range) 0) (archer_auto_mv))
		  (else (other_auto_mv))
		  )
		 (yeSetIntAt fwid "cur_target" -1)
		 (cond
		  ((< (range) 0) (mk_archer_targets))
		  (else (mk_other_targets))
		  )
		 (find_target)
		 (if (can_atk) (do_atk_))
		 (clr_mv_t)
		 (yeClearArray (mv_trace))
		 ))

	       (enemies_turn
		(lambda ()
		  (yeReCreateArray mb "mv_trace")
		  (yeReCreateArray mb "targets")
		  (yeForeach (c_units)
			     (lambda (el a)
			       (yeSetIntAt fwid "cur_unit"
					   (yeArrayIdx_ent (c_units) el))
			       (soldier_of_ai)
			       ))
					; reset player stats V
		  (yeForeach (t_units)
			     (lambda (el a)
			       (yeSetIntAt el "have_attack" 0)
			       (yeSetIntAt el "mv" 5)
			       ))
		  (yeSetIntAt fwid "cur_unit" 0)
		  (yeSetIntAt fwid "cur_player" 0)
		  (map_out)
		  ))
	       )
	(cond ((= (m_state) medba_map_choose_unit_state) (choose_guy))
	      ((= (m_state) medba_map_move_state) (mv_guy))
	      ((= (m_state) medba_map_enemy_turn) (enemies_turn))
	      ((= (m_state) medba_map_atk_state) (atk_guy)))
	)
      )
    )

  (define medba_endturn
    (lambda (mn eves)
      (let(
	   (mbc (ywCntWidgetFather mn))
	   )
	(begin
	  (yeSetIntAt mbc "cur_player" 1)
	  (yeSetIntAt mbc "cur_unit" 0)
	  (yeSetIntAt mbc "current" 0)
	  (yeSetIntAt mbc "map_state" medba_map_enemy_turn)
	  )
	)
      )
    )

  (define medba_tomapmv
    (lambda (mn eves)
      (begin
	(yeSetIntAt (ywCntGetEntry (ywCntWidgetFather mn) 0) "action_type" 0)
	(yeSetIntAt (ywCntWidgetFather mn) "current" 0)
	)
      )
    )
  (define medba_tomapatk
    (lambda (mn eves)
      (begin
	(yeSetIntAt (ywCntGetEntry (ywCntWidgetFather mn) 0) "action_type" 1)
	(yeSetIntAt (ywCntWidgetFather mn) "current" 0)
	)
      )
    )

  (define medba_destroy
    (lambda (c)
      (ywSetTurnLengthOverwrite (yeGetIntAt c "otime"))
      (ygModDirOut)
      )
    )



  (define medba_dinit
   (lambda (wid unused)
      (letrec ((mk_dumb_rend
		(lambda (r_info y)
		  (ywRectCreate 5 (+ 1032 y) 45 60 r_info "src")
		  (ywPosCreate (- 11) 0 r_info "threshold")
		  ))
	       (mob_init
		(lambda (mb t hp range atk rid)
		  (begin
		    (yeCreateString t mb "type")
		    (yeCreateInt hp mb "hp")
		    (yeCreateInt atk mb "atk")
		    (yeCreateInt range mb "range")
		    (yeCreateInt rid mb "id")
		    (ywPosCreate 0 0 mb "pos")
		    (yeCreateInt 5 mb "mv")
		    (yeCreateInt 0 mb "have_attack")
		    (mk_dumb_rend (yeCreateArray mb "rend_info0") 0)
		    (mk_dumb_rend (yeCreateArray mb "rend_info1") 130)
		    )))
	       (mob_init_all
		(lambda (wid mobs)
		  (begin
		    (mob_init (yeCreateArray mobs) "archer"   6  -1 2 5)
		    (mob_init (yeCreateArray mobs) "spearman" 10  2 4 4)
		    (mob_init (yeCreateArray mobs) "swordman" 13  1 5 3)
		    (mob_init (yeCreateArray mobs) "spearman" 10  2 4 4)
		    (mob_init (yeCreateArray mobs) "archer"   6  -1 2 5)
		    (yeCreateCopy mobs wid "p0")
		    (yeCreateCopy mobs wid "p1")
		    wid
		    ))))
	(medba_init (mob_init_all wid (yeCreateArray)) ())
	)
      )
   )

   (define medba_init
    (lambda (wid unused_type)
      (letrec
	  (
	   (mk_elem
	    (lambda (el c p n)
	      (begin
		(yeCreateString c el "map-char")
		(yeCreateString p el "map-sprite")
		(yeCreateString n el "name")
		)))
	   (mk_resources
	    (lambda (ra)
	      (begin
		;(mk_elem (yeCreateArray ra) "." "./floor.png" "floor")
		(yeCreateString "." (yeCreateArray ra) "map-char")
		(yeCreateString "rgba: 255 127 13 127"
				(yeCreateArray ra) "map-color")
		(yeCreateString "rgba: 127 255 127 127"
				(yeCreateArray ra) "map-color")
		(mk_elem (yeCreateArray ra) "W" "./sword.png" "swordman")
		(mk_elem (yeCreateArray ra) "S" "./spear.png" "spearman")
		(mk_elem (yeCreateArray ra) "A" "./archer.png" "archer")
		(mk_elem (yeCreateArray ra) "^" "./arrow_up.png" "arrow_up")
		(mk_elem (yeCreateArray ra) "v" "./arrow_down.png" "arrow_down")
		ra)))
	   (init_medba
	    (lambda (mb mbc)
	      (begin
		(yeCreateString "map" mb "<type>")
		(ywMapInitEntity mb (mk_resources (yeCreateArray)) 0
				 medba_map_w medba_map_h)
		(yeCreateInt 80 mb "size")
		(yeCreateInt 0 mb "action_type")
		(yeCreateFunction "medba_action" mb "action")
		(yeCreateString "rgba: 255 255 255 255" mb "background")
		(ywPosCreate 0 0 mb "c_c_p")
		(yeForeach (yeGet mbc "p0")
			   (lambda (el arg)
			     (yePush el (yeGet el "rend_info0") "rend_info")
			     (ywPosSetInts (yeGet el "pos")
					   (+ (* (cdr arg) 2) 3)
					   (- medba_map_h 1) )
			     (yeCreateInt 0 el "player")
			     (ywMapPushElem (car arg) el (yeGet el "pos") "p0u")
			     (cons (car arg) (+ (cdr arg) 1))
			     ) (cons mb 0))
		(yeForeach (yeGet mbc  "p1")
			   (lambda (el arg)
			     (yePush el (yeGet el "rend_info1") "rend_info")
			     (ywPosSetInts (yeGet el "pos")
					   (+ (* (cdr arg) 2) 3) 1 )
			     (yeCreateInt 1 el "player")
			     (ywMapPushElem (car arg) el (yeGet el "pos") "p1u")
			     (cons (car arg) (+ (cdr arg) 1))
			     ) (cons mb 0))

		mb)))
	   (init_medba_mn
	    (lambda (mbm mbc)
	      (begin
		(yeCreateString "menu" mbm "<type>")
		(yeCreateString "rgba: 127 127 127 255" mbm "background")
		(yeCreateString "" mbm "pre-text")
		(ywMenuPushEntry mbm "Move"
				 (yeCreateFunction "medba_tomapmv"))
		(ywMenuPushEntry mbm "Attack"
				 (yeCreateFunction "medba_tomapatk"))
		(ywMenuPushEntry mbm "End Turn"
				 (yeCreateFunction "medba_endturn"))
		(ywMenuPushEntry mbm "End Game"
				 (if (yeIsChild mbc "quit")
				     (yeGet mbc "quit")
				     (yeCreateString "FinishGame")))
		mbm
		))
	   )
	   (init_medba_cnt
	    (lambda (mbc)
	      (begin
		(ygModDir "medieval_battle")
		(yeCreateInt 0 mbc "cur_player")
		(yeCreateFunction "medba_destroy" mbc "destroy")
		(yeCreateInt (ywGetTurnLengthOverwrite) mbc "otime")
		(ywSetTurnLengthOverwrite 0)
		(yeCreateInt medba_map_choose_unit_state mbc "map_state")
		(yeCreateString "vertical" mbc "cnt-type")
		(yeCreateInt 0 mbc "cur_unit")
		(yeCreateInt -1 mbc "cur_target")
		(ywPushNewWidget mbc (init_medba (yeCreateArray) mbc))
		(ywPushNewWidget mbc (init_medba_mn (yeCreateArray) mbc))
		mbc
		)))
	)
	(ywidNewWidget (init_medba_cnt wid) "container")
      )
      )
    )


  (define mod_init
    (lambda (mod)
      (letrec ((init (yeCreateArray) )
	       (init2 (yeCreateArray) )
	       )
	(begin
	  (yeCreateString "medieval_battle" mod "name")
	  (ywSizeCreate 800 600 mod "window size")
	  (yeCreateString "test" mod "starting widget")
	  (yeCreateFunction "medba_init" init "callback")
	  (yeCreateString "medieval_battle" init "name")
	  (yeCreateFunction "medba_dinit" init2 "callback")
	  (yeCreateString "medieval_dbattle" init2 "name")
	  (yeCreateString "medieval_dbattle" (yeCreateArray mod "test") "<type>")
	  (ywidAddSubType init)
	  (ywidAddSubType init2)
	  mod
	  )
	)
      )
    )

  )
