(begin

  (define medba_map_w 15)
  (define medba_map_h 20)

  (define medba_map_choose_unit_state 0)
  (define medba_map_move_state 1)

  (define medba_action
    (lambda (mb eves)
      (letrec(
	      (fwid (ywCntWidgetFather mb))
	      (cur_pl (yeGetIntAt fwid "cur_player"))
	      (cur_u_idx (yeGetIntAt fwid "cur_unit"))
	      (p_units (lambda () (if (= cur_pl 0)
				   (yeGet fwid "p0")
				   (yeGet fwid "p1"))))
	      (m_state (yeGetIntAt fwid "map_state"))
	      (c_u (lambda () (yeGet (p_units) cur_u_idx)))
	      (c_u_p (lambda () (yeGet (c_u) "pos")))
	      (choose_guy (lambda ()
		(begin
		  (display "wesh ")
		  (display m_state)
		  (display cur_pl)
		  (display cur_u_idx)
		  (display (c_u))
		  (display (c_u_p))
		  (display "\n")
		  (ywMapPushElem mb (yeCreateInt 2) (c_u_p) "cursor")
		  (if (yevIsKeyDown eves Y_ESC_KEY)
		      (yeSetIntAt fwid "current" 1))
		  )
		))
	      )
	(begin
	  (if (= m_state medba_map_choose_unit_state)
	      (choose_guy))
	  )
	)
      )
    )

  (define medba_endturn
    (lambda (mn eves)
      (display "\nMEDBA ENDTURN \n\n")
      )
    )

  (define medba_tomap
    (lambda (mn eves)
      (begin
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
		(yeCreateString p el "map-tild")
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
		(mk_elem (yeCreateArray ra) "W" "./swordman.png" "swordman")
		(mk_elem (yeCreateArray ra) "S" "./spearman.png" "spearman")
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
		(yeCreateFunction "medba_action" mb "action")
		(yeCreateString "rgba: 255 255 255 255" mb "background")
		(yeForeach (yeGet mbc "p0")
			   (lambda (el arg)
			     (ywPosSetInts (yeGet el "pos")
					   (+ (* (cdr arg) 2) 3)
					   (- medba_map_h 1) )
			     (yeCreateInt 0 el "player")
			     (ywMapPushElem (car arg) el (yeGet el "pos"))
			     (cons (car arg) (+ (cdr arg) 1))
			     ) (cons mb 0))
		(yeForeach (yeGet mbc  "p1")
			   (lambda (el arg)
			     (ywPosSetInts (yeGet el "pos")
					   (+ (* (cdr arg) 2) 3) 0 )
			     (yeCreateInt 1 el "player")
			     (ywMapPushElem (car arg) el (yeGet el "pos"))
			     (cons (car arg) (+ (cdr arg) 1))
			     ) (cons mb 0))

		mb)))
	   (init_medba_mn
	    (lambda (mbm mbc)
	      (begin
		(yeCreateString "menu" mbm "<type>")
		(yeCreateString "rgba: 127 127 127 255" mbm "background")
		(ywMenuPushEntry mbm "Move/Attack"
				 (yeCreateFunction "medba_tomap"))
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
	       (mob_init
		(lambda (mb t hp range rid)
		  (begin
		    (yeCreateString t mb "type")
		    (yeCreateInt hp mb "hp")
		    (yeCreateInt range mb "range")
		    (yeCreateInt rid mb "id")
		    (ywPosCreate 0 0 mb "pos")
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
