"! <p class="shorttext synchronized" lang="de">LEO 3.0 HX Inbound Port for BGRFC</p>
interface /DKVES/KP_IF_TVID_BGRFC_IB_ADP
  public .


  "! <p class="shorttext synchronized" lang="de">
  "! Executes Video Tolling Ordering Use Case</p>
  methods EXECUTE_VIDEO_TOLLING_ORDER
    importing
      !IS_ORDER type /DKVES/KP_MTR_VID_TOLL_ORDER_S
    returning
      value(RT_RESULT) type BAPIRET2_TT
    raising
      /DKVES/UE_CX_HX_T100_DYNAMIC.

  " Method for termination a video tolling service
  "! <p class="shorttext synchronized" lang="de">
  "! Executes Video Tolling Termination Use Case</p>
  methods EXECUTE_VIDEO_TOLLING_TERM
    importing
      !IS_TERMINATION type /DKVES/KP_MTR_VIDEO_TERM_RQ_S
    returning
      value(RT_RESULT) type BAPIRET2_TT
    raising
      /DKVES/UE_CX_HX_T100_DYNAMIC .
endinterface.
