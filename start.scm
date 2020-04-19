(begin

  (define medba_map_w 15)
  (define medba_map_h 20)

  (define medba_map_choose_unit_state 0)
  (define medba_map_move_state 1)
  (define medba_map_atk_state 2)

  (define medba_action
    (lambda (mb eves)
      (letrec(
	      (fwid (ywCntWidgetFather mb))
	      (mbm (ywCntGetEntry (ywCntWidgetFather mb) 1))
	      (cur_pl (lambda () (yeGetIntAt fwid "cur_player")))
	      (cur_u_idx (lambda () (yeGetIntAt fwid "cur_unit")))
	      (p_units (lambda () (if (= (cur_pl) 0)
				   (yeGet fwid "p0")
				   (yeGet fwid "p1"))))
	      (m_state (lambda () (yeGetIntAt fwid "map_state")))
	      (c_u (lambda () (yeGet (p_units) (cur_u_idx))))
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
		 (yeForeach (yeGet fwid "p1")
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

	      (mvp (lambda () (yeGetIntAt (c_u) "mv")))
	      (have_atk (lambda () (yeGetIntAt (c_u) "have_attack")))

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
		 (clr_mv_t)
		 (yeClearArray (mv_trace))
		 (map_out)))

	      (mv_u
	       (lambda (to)
		 (clr_mv_t)
		 (yeForeach (mv_trace)
			    (lambda (el a)
			      (display el)
			      (ywMapMoveByEntity mb (c_u_p) el (c_u))
			      (ywPosSet (c_u_p) el)
			      (ywidRendMainWid)
			      (yuiUsleep 300000)
			      ))
		 (ywMapMoveByEntity mb (c_u_p) to (c_u))
		 (ywPosSet (c_u_p) to)
		 (if (not (= (mvp) 0))
		     (yeAddAt (c_u) "mv" (- (- (yeLen (mv_trace)) 1))))
		 ))

	      (atk_guy
	       (lambda ()
		 (letrec ()
		   (begin
		     (display (yeGet (c_u) "range"))
		     (display (yeLen (targets)))
		     (display " attacking\n")
		     (if (yevIsKeyDown eves Y_ESC_KEY)
			 (mv_guy_next_out))
		     )
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
			   (mk_u_nfo (yeGet mbm "pre-text"))
			   )))
		      (mv_p
		       (lambda (xy)
			 (begin
			   (ywMapPushElem mb (yeCreateInt 2) (c_c_p) "mv-trace")
			   (yeCreateCopy (c_c_p) (mv_trace))
			   (mk_u_nfo (yeGet mbm "pre-text"))
			   (ywPosSet (c_c_p) (car xy) (cdr xy))
			   )))
		      (chk_case
		       (lambda (xy)
			 (if (ywMapContainEnt mb (car xy) (cdr xy) (c_u)) 0
			     (if (ywMapContainStr mb (car xy) (cdr xy) "p0u") 1
				 (if (ywMapContainStr mb (car xy) (cdr xy) "p1u") 2 0)))
			 )
		       )
		      (chk_can_mv
		       (lambda (xy)
			 (if (= (chk_case xy) 0)
			     (if (ywPosIsSame (l_trc)
					      (car xy) (cdr xy))
				 (restore_p xy)
				 (if (< (yeLen (mv_trace)) (+ (mvp) 1))
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
	      (mk_u_nfo (lambda (str)
			  (begin
			    (yeSetString str "")
			    (yeStringAdd str "unit type:")
			    (yeAddEnt str (yeGet (c_u) "type"))
			    (yeStringAdd str "\nHP:")
			    (yeStringAddInt str (yeGetIntAt (c_u) "hp"))
			    (yeStringAdd str "\nMove Point:")
			    (yeStringAddInt str (- (+ (mvp) 1)
						   (if (= (yeLen (mv_trace)) 0) 1
						       (yeLen (mv_trace)))))
			    (if (= (have_atk) 0)
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
		  (mk_u_nfo (yeGet mbm "pre-text"))
		  (if (yevIsKeyDown eves Y_ESC_KEY)
		      (map_out)
		      (ywMapPushElem mb (yeCreateInt 2) (c_u_p) "cursor")
		      )
		  (if (and (yevIsKeyDown eves Y_ENTER_KEY) (= (have_atk) 0))
		      (choose_guy_next_state))
		  (display (yeGetIntAt fwid "map_state"))
		  (display "\n")
		  )
		))
	      )
	(cond ((= (m_state) medba_map_choose_unit_state) (choose_guy))
	      ((= (m_state) medba_map_move_state) (mv_guy))
	      ((= (m_state) medba_map_atk_state) (atk_guy)))
	)
      )
    )

  (define medba_endturn
    (lambda (mn eves)
      (display "\nMEDBA ENDTURN \n\n")
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
		(mk_elem (yeCreateArray ra) "." "./floor.png" "floor")
		(yeCreateString "rgba: 255 127 13 127"
				(yeCreateArray ra) "map-color")
		(yeCreateString "rgba: 127 255 127 127"
				(yeCreateArray ra) "map-color")
		(mk_elem (yeCreateArray ra) "W" "./sword--.png" "swordman")
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
		(ywMenuPushEntry mbm "End Game" (yeCreateString "FinishGame"))
		mbm
		))
	   )
	   (init_medba_cnt
	    (lambda (mbc)
	      (begin
		(yeCreateInt 0 mbc "cur_player")
		(yeCreateInt medba_map_choose_unit_state mbc "map_state")
		(yeCreateString "vertical" mbc "cnt-type")
		(yeCreateInt 0 mbc "cur_unit")
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
	       (mobs (yeCreateArray) )
	       (mk_dumb_rend
		(lambda (r_info y)
		  (ywRectCreate 5 (+ 1032 y) 45 60 r_info "src")
		  (ywPosCreate (- 11) 0 r_info "threshold")
		    ))
	       (mob_init
		(lambda (mb t hp range rid)
		  (begin
		    (yeCreateString t mb "type")
		    (yeCreateInt hp mb "hp")
		    (yeCreateInt range mb "range")
		    (yeCreateInt rid mb "id")
		    (ywPosCreate 0 0 mb "pos")
		    (yeCreateInt 5 mb "mv")
		    (yeCreateInt 0 mb "have_attack")
		    (mk_dumb_rend (yeCreateArray mb "rend_info0") 0)
		    (mk_dumb_rend (yeCreateArray mb "rend_info1") 130)
		    )))
	       (init_test_wid
		(lambda (wid)
		  (begin
		    (yeCreateString "medieval_battle" wid "<type>")
		    (mob_init (yeCreateArray mobs) "archer" 6 -1 5)
		    (mob_init (yeCreateArray mobs) "spearman" 10 2 4)
		    (mob_init (yeCreateArray mobs) "swordman" 13 1 3)
		    (mob_init (yeCreateArray mobs) "spearman" 10 2 4)
		    (mob_init (yeCreateArray mobs) "archer" 6 -1 5)
		    (yeCreateCopy mobs wid "p0")
		    (yeCreateCopy mobs wid "p1")
		    wid
		    ))))
	(begin
	  (yeCreateString "medieval_battle" mod "name")
	  (yeCreateString "test" mod "starting widget")
	  (yeCreateFunction "medba_init" init "callback")
	  (yeCreateString "medieval_battle" init "name")
	  (init_test_wid (yeCreateArray mod "test"))
	  (ywidAddSubType init)
	  mod
	  )
	)
      )
    )

  )
