CLASS /dkves/kp_cl_tvid_bgrfc_ib_adp DEFINITION
  PUBLIC
  INHERITING FROM /dkves/ue_cl_hx_comp_base
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS /dkves/ue_if_hx_comp_creator .

  PUBLIC SECTION.

    INTERFACES /dkves/ue_if_hx_ib_adapter.
    INTERFACES /dkves/kp_if_tvid_bgrfc_ib_adp.

  PRIVATE SECTION.
    DATA mo_use_case TYPE REF TO /dkves/ue_if_hx_use_case.
    DATA mo_order_uc TYPE REF TO /dkves/kp_if_tvid_order_ib_prt.
    DATA mo_term_uc  TYPE REF TO /dkves/kp_if_tvid_term_ib_prt.
ENDCLASS.



CLASS /dkves/kp_cl_tvid_bgrfc_ib_adp IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_bgrfc_ib_adp~execute_video_tolling_order.
    mo_order_uc->execute_video_tolling_order( is_order ).
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_bgrfc_ib_adp~execute_video_tolling_term.
    mo_term_uc->execute_video_tolling_term( is_termination  ).
  ENDMETHOD.


  METHOD /dkves/ue_if_hx_ib_adapter~set_use_case.
    "Here would be better to have two BGRFC IB Adapter
    "One for Order and another for Temrination Use Case
    TRY.
     "Is it the Order Use Case?
        mo_order_uc ?= io_use_case.

      CATCH cx_sy_move_cast_error.

        TRY."Try with Termination
            mo_term_uc ?= io_use_case.

          CATCH cx_sy_move_cast_error INTO DATA(lo_error).
            RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
              EXPORTING
                previous = lo_error.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
