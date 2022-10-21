! Edited by Sun Chao at 2022-07 
! 


module atmocn_flux_ccpl_kinds

    use shr_kind_mod,      only: r8 => shr_kind_r8, in=>shr_kind_in

    implicit none

    integer, public  :: atmocn_flux_ccpl_comm, atmocn_flux_comp_id, atmocn_flux_grid_id, atmocn_flux_instance_id, atmocn_flux_decomp_id
    integer, public  :: ccpl_dead_comps, ccpl_nloc, ccpl_npe_all, ccpl_mype_all
    integer, public  :: field_id_xao_So_tref, field_id_xao_So_qref, field_id_xao_So_ustar, field_id_xao_So_re, field_id_xao_So_ssq, field_id_xao_So_u10, &
                        field_id_xao_So_duu10n, field_id_xao_Faox_taux, field_id_xao_Faox_tauy, field_id_xao_Faox_lat, field_id_xao_Faox_sen, field_id_xao_Faox_evap, &
                        field_id_xao_Faox_lwup, field_id_a2x_Sa_z, field_id_a2x_Sa_u, field_id_a2x_Sa_v, field_id_a2x_Sa_tbot, field_id_a2x_Sa_ptem, field_id_a2x_Sa_shum, &
                        field_id_a2x_Sa_dens, field_id_o2x_So_t, field_id_o2x_So_u, field_id_o2x_So_v, field_id_mask
    integer(in), dimension(:), pointer, public   :: ccpl_mask=>null()
    real(r8), dimension(:), pointer, public :: ccpl_xao_So_tref=>null(), ccpl_xao_So_qref=>null(), ccpl_xao_So_ustar=>null(), ccpl_xao_So_re=>null(), &
                                           ccpl_xao_So_ssq=>null(), ccpl_xao_So_u10=>null(), ccpl_xao_So_duu10n=>null(), ccpl_xao_Faox_taux=>null(), &
                                           ccpl_xao_Faox_tauy=>null(), ccpl_xao_Faox_lat=>null(), ccpl_xao_Faox_sen=>null(), ccpl_xao_Faox_evap=>null(), &
                                           ccpl_xao_Faox_lwup=>null(), ccpl_a2x_Sa_z=>null(), ccpl_a2x_Sa_u=>null(), ccpl_a2x_Sa_v=>null(), ccpl_a2x_Sa_tbot=>null(), &
                                           ccpl_a2x_Sa_ptem=>null(), ccpl_a2x_Sa_shum=>null(), ccpl_a2x_Sa_dens=>null(), ccpl_o2x_So_t=>null(), ccpl_o2x_So_u=>null(), &
                                           ccpl_o2x_So_v=>null()
    integer(in),allocatable, public :: emask(:) ! ocn mask on exchange grid decomp
    real(r8), allocatable, public ::  uocn (:)  ! ocn velocity, zonal
    real(r8), allocatable, public ::  vocn (:)  ! ocn velocity, meridional
    real(r8), allocatable, public ::  tocn (:)  ! ocean temperature
    real(r8), allocatable, public ::  zbot (:)  ! atm level height
    real(r8), allocatable, public ::  ubot (:)  ! atm velocity, zonal     
    real(r8), allocatable, public ::  vbot (:)  ! atm velocity, meridional
    real(r8), allocatable, public ::  thbot(:)  ! atm potential T
    real(r8), allocatable, public ::  shum (:)  ! atm specific humidity
    real(r8), allocatable, public ::  dens (:)  ! atm density
    real(r8), allocatable, public ::  tbot (:)  ! atm bottom surface T
    real(r8), allocatable, public ::  sen  (:)  ! heat flux: sensible 
    real(r8), allocatable, public ::  lat  (:)  ! heat flux: latent   
    real(r8), allocatable, public ::  lwup (:)  ! lwup over ocean
    real(r8), allocatable, public ::  evap (:)  ! water flux: evaporation
    real(r8), allocatable, public ::  taux (:)  ! wind stress, zonal
    real(r8), allocatable, public ::  tauy (:)  ! wind stress, meridional
    real(r8), allocatable, public ::  tref (:)  ! diagnostic:  2m ref T
    real(r8), allocatable, public ::  qref (:)  ! diagnostic:  2m ref Q
    real(r8), allocatable, public :: duu10n(:)  ! diagnostic: 10m wind speed squared
  
    real(r8), allocatable, public ::  ustar(:)  ! saved ustar
    real(r8), allocatable, public ::  re   (:)  ! saved re
    real(r8), allocatable, public ::  ssq  (:)  ! saved sq

end module

