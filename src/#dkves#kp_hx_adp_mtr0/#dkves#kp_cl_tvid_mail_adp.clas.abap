class /DKVES/KP_CL_TVID_MAIL_ADP definition
  public
  inheriting from /DKVES/UE_CL_HX_COMP_BASE
  final
  create private

  global friends /DKVES/UE_IF_HX_COMP_CREATOR .

public section.

  interfaces /dkves/kp_if_tvid_mail_ob_pRT .
  PRIVATE SECTION.

ENDCLASS.



CLASS /DKVES/KP_CL_TVID_MAIL_ADP IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_mail_ob_pRT~register_email.
    " Mail an Kunden erzeugen/erweitern
    DATA(lr_mail_registry) = /dkves/ue_cl_email_registry=>factory( ).

    lr_mail_registry->add_email(
        is_email = VALUE #( type  = /dkves/if_ue_email_type=>tr_order_confirmation
                            kunnr =  iv_customer
                            business_references
                                = VALUE #( ( table_name = '/DKVES/KP_BESTEK'
                                             table_key = sy-mandt && iv_order_nr ) ) ) ).

    lr_mail_registry->save_in_update_task( ).
  ENDMETHOD.
ENDCLASS.
