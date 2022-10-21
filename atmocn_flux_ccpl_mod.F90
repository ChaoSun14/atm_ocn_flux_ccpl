! Edited by Sun Chao at 2022-07
! 

module atmocn_flux_ccpl_mod

  use CCPL_interface_mod ! c-coupler module
  use atmocn_flux_ccpl_kinds
  use atmocn_flux_ccpl_coupling_mod
  implicit none

  public :: atmocn_flux_ccpl_init
  public :: atmocn_flux_ccpl_run
  public :: atmocn_flux_ccpl_finalize


  contains

!==============================================================================  
! DESCRIPTION: This module contains the atmocn_flux main program, which has been split
! into initialize/run/finalize segments, and subroutines created for these steps:
! atmocn_flux_initialize(), atmocn_flux_run() and atmocn_flux_finalize(). 
!==============================================================================


   subroutine atmocn_flux_ccpl_init(ccpl_procedure_inst_id) bind(c)

       implicit none
       integer, intent(in)    ::  ccpl_procedure_inst_id
       atmocn_flux_instance_id = ccpl_procedure_inst_id
       atmocn_flux_ccpl_comm = CCPL_external_modules_get_local_comm(ccpl_procedure_inst_id, annotation="get local comm")
       call atmocn_flux_ccpl_coupling_registration

   end subroutine atmocn_flux_ccpl_init

   subroutine atmocn_flux_ccpl_run() bind(c)

      implicit none
     
      call ccpl_atmocn_flux_run

   end subroutine atmocn_flux_ccpl_run

    subroutine atmocn_flux_ccpl_finalize() bind(c)

        implicit none

    end subroutine atmocn_flux_ccpl_finalize

  end module atmocn_flux_ccpl_mod

