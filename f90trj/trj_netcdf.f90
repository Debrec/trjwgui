!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!! Read meteorological data for netcdf ecmwf files
MODULE trj_netcdf
  USE netcdf
  IMPLICIT none
  PRIVATE
  PUBLIC :: read_ecmwf_nc_file, dim_ecmwf_nc_file,read_dim_ecmwf_nc_file, inq_var

  INTEGER :: ncid, status, varid
  INTEGER :: nvar, ndim, natts

  INTERFACE dim_ecmwf_nc_file
     MODULE PROCEDURE dim_ecmwf_nc_file_md
     MODULE PROCEDURE dim_ecmwf_nc_file_ud
  END INTERFACE

  INTERFACE read_ecmwf_nc_file
     MODULE PROCEDURE read_ecmwf_nc_file_ps
     MODULE PROCEDURE read_ecmwf_nc_file_f
  END INTERFACE

  INTERFACE read_dim_ecmwf_nc_file
     MODULE PROCEDURE read_dim_ecmwf_nc_file_s
     MODULE PROCEDURE read_dim_ecmwf_nc_file_f
  END INTERFACE

CONTAINS

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    ! Part of mo_netcdf.f90, MPI
    INTEGER :: status
    IF (status /= nf90_noerr) THEN

       WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE nf

  SUBROUTINE dim_ecmwf_nc_file_md(filename,namedim,nro,nrodim)
    CHARACTER(*), INTENT(IN) :: filename, namedim(:)
    INTEGER, INTENT(IN) :: nrodim
    INTEGER, INTENT(OUT) :: nro(:)

    INTEGER Jdim,len

    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

    DO Jdim=1,nrodim
       CALL nf(NF90_INQ_VARID(ncid,namedim(Jdim),varid))
       CALL nf(NF90_INQUIRE_DIMENSION(ncid,varid,len=len))
       WRITE(*,*) namedim(Jdim), len
       nro(Jdim)=len
    END DO

    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE dim_ecmwf_nc_file_md

  SUBROUTINE dim_ecmwf_nc_file_ud(filename,namedim,nro)
    CHARACTER(*), INTENT(IN) :: filename, namedim
    INTEGER, INTENT(OUT) :: nro

    INTEGER len

       CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

         CALL nf(NF90_INQ_VARID(ncid,namedim,varid))
         CALL nf(NF90_INQUIRE_DIMENSION(ncid,varid,len=len))
         WRITE(*,*) namedim, len
         nro=len

       CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE dim_ecmwf_nc_file_ud

  SUBROUTINE read_dim_ecmwf_nc_file_f(filename,namedim,datadim,nro,nrodim)
       CHARACTER(*), INTENT(IN) :: filename, namedim(:)
       INTEGER, INTENT(IN) :: nrodim, nro(:)
       REAL, INTENT(OUT) :: datadim(:,:)

       INTEGER Jdim,I,len

       CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

       DO Jdim=1,nrodim
         CALL nf(NF90_INQ_VARID(ncid,namedim(Jdim),varid))
         CALL nf(NF90_GET_VAR(ncid,varid,datadim(Jdim,1:nro(Jdim))))
       !  WRITE(*,*) namedim(Jdim)
       ! WRITE(*,*)(datadim(Jdim,I),I=1,nro(Jdim))
       END DO

       CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE read_dim_ecmwf_nc_file_f

  SUBROUTINE read_dim_ecmwf_nc_file_s(filename,namedim,datadim,nro)
       CHARACTER(*), INTENT(IN) :: filename, namedim
       INTEGER, INTENT(IN) :: nro
       REAL, INTENT(OUT) :: datadim(:)

       INTEGER Jdim,I,len

       CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

         CALL nf(NF90_INQ_VARID(ncid,namedim,varid))
         CALL nf(NF90_GET_VAR(ncid,varid,datadim(1:nro)))

       CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE read_dim_ecmwf_nc_file_s


  SUBROUTINE read_ecmwf_nc_file_ps(filename,varname,var,units,nro,itime,etime)
    IMPLICIT none
    CHARACTER(*), INTENT(IN) :: filename, varname
    INTEGER, INTENT(IN) :: nro(:)
    REAL, INTENT(OUT) :: var(nro(1),nro(2),nro(3),2)
    CHARACTER(*), INTENT(OUT) :: units
    INTEGER, INTENT(IN), OPTIONAL :: itime, etime

    INTEGER :: I,J,len
    REAL, ALLOCATABLE :: ivar(:,:,:,:)
    REAL scale_factor,add_offset
    INTEGER :: start3d(4),cnt3d(4)


    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))
    IF(PRESENT(itime).AND.PRESENT(etime)) THEN
       ALLOCATE(ivar(nro(1),nro(2),nro(3),etime-itime+1))
       start3d = (/1,1,1,itime /)
       cnt3d = (/nro(1), nro(2), nro(3), etime-itime+1 /)
    ELSE
       ALLOCATE(ivar(nro(1),nro(2),nro(3),nro(4)))
       start3d = (/1,1,1,1/)
       cnt3d = (/nro(1), nro(2), nro(3), nro(4) /)
    END IF

    CALL nf(NF90_INQ_VARID(ncid,varname, varid))
    CALL nf(NF90_GET_VAR(ncid,varid,ivar(:,:,:,:),start3d,cnt3d))
    CALL nf(NF90_GET_ATT(ncid,varid,"add_offset",add_offset))
    CALL nf(NF90_GET_ATT(ncid,varid,"scale_factor",scale_factor))
    CALL nf(NF90_GET_ATT(ncid,varid,"units",units))
    var(:,:,:,:)=scale_factor*ivar(:,:,:,:)+add_offset

    CALL nf(NF90_CLOSE(ncid))

    DEALLOCATE(ivar)

  END SUBROUTINE read_ecmwf_nc_file_ps

  SUBROUTINE read_ecmwf_nc_file_f(filename,varname,var,units,nro,nrovar)

    IMPLICIT none

    CHARACTER(*), INTENT(IN) :: filename, varname(:)
    INTEGER, INTENT(IN) :: nro(:), nrovar
    REAL, INTENT(OUT) :: var(nrovar,nro(1),nro(2),nro(3),nro(4))
    CHARACTER(*), INTENT(OUT) :: units(:)

    INTEGER :: I,J,len,Jvar
    REAL :: ivar(nrovar,nro(1),nro(2),nro(3),nro(4))
    REAL scale_factor,add_offset

    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

    DO Jvar=1,nrovar
       CALL nf(NF90_INQ_VARID(ncid,varname(Jvar), varid))
       CALL nf(NF90_GET_VAR(ncid,varid,ivar(Jvar,:,:,:,:)))
       CALL nf(NF90_GET_ATT(ncid,varid,"add_offset",add_offset))
       CALL nf(NF90_GET_ATT(ncid,varid,"scale_factor",scale_factor))
       CALL nf(NF90_GET_ATT(ncid,varid,"units",units(Jvar)))
       var(Jvar,:,:,:,:)=scale_factor*ivar(Jvar,:,:,:,:)+add_offset
    END DO



    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE read_ecmwf_nc_file_f



!  SUBROUTINE read_ecmwf_nc_file_s(filename,varname,var,units,nro)
!
!    IMPLICIT none
!
!    CHARACTER(*), INTENT(IN) :: filename, varname
!    INTEGER, INTENT(IN) :: nro(:)
!    REAL, INTENT(OUT) :: var(:,:,:,:)
!    CHARACTER(*), INTENT(OUT) :: units
!
!    INTEGER :: I,J,len
!    REAL, ALLOCATABLE :: ivar(:,:,:,:)
!    REAL scale_factor,add_offset
!
!    ALLOCATE(ivar(nro(1),nro(2),nro(3),nro(4)))

!    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

!    CALL nf(NF90_INQ_VARID(ncid,varname, varid))
!       CALL nf(NF90_GET_VAR(ncid,varid,ivar(:,:,:,:)))
!       CALL nf(NF90_GET_ATT(ncid,varid,"add_offset",add_offset))
!       CALL nf(NF90_GET_ATT(ncid,varid,"scale_factor",scale_factor))
!       CALL nf(NF90_GET_ATT(ncid,varid,"units",units))
       !WRITE(*,*) varname(Jvar) , add_offset, scale_factor, units(Jvar)
       !WRITE(*,*)(((scale_factor*ivar(Jvar,I,J,1)+add_offset),I=1,144),J=1,73)
!       var(:,:,:,:)=scale_factor*ivar(:,:,:,:)+add_offset

!    CALL nf(NF90_CLOSE(ncid))

!    DEALLOCATE(ivar)

!  END SUBROUTINE read_ecmwf_nc_file_s


  SUBROUTINE inq_var(filename)
   IMPLICIT none

    CHARACTER(*), INTENT(IN) ::filename
    INTEGER :: I, J,len,ngatts , type
    CHARACTER*50 :: name
    CHARACTER*100 conv

    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))
    CALL nf(NF90_INQUIRE(ncid,nVariables=nvar,nDimensions=ndim,nAttributes=ngatts))
       WRITE(*,*) 'globals'
       CALL nf(NF90_INQUIRE_ATTRIBUTE(ncid,NF90_GLOBAL,name="Conventions",len=len))
        WRITE(*,*) '  ', len
       CALL nf(NF90_GET_ATT(ncid,NF90_GLOBAL,"Conventions",conv))
       WRITE(*,*) conv
    !END DO
    DO I=1,nvar
      CALL nf(NF90_INQUIRE_VARIABLE(ncid,I,name=name,natts=natts))
      WRITE(*,*) name,natts
      DO J=1,natts
        CALL nf(NF90_INQ_ATTNAME(ncid,I,J,name))
        WRITE(*,*) '  ',I,name
      END DO
    END DO
    DO I=1,ndim
       CALL nf(NF90_INQUIRE_DIMENSION(ncid,I,name=name,len=len))
       WRITE(*,*) name,len
    END DO
    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE inq_var

END MODULE trj_netcdf
!****************************************************************************

!////////////////////////////////////////////////////////////////////////////

!****************************************************************************
!! trj_netcdf_w, write n-dimensional data to netcdf file
MODULE trj_netcdf_w
  ! base in the module mo_netcdf.f90 de echam5
  USE netcdf
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: open_trj_nc_file, write_trj_nc_file, close_trj_nc_file

  ! choose a precision:
  INTEGER, PARAMETER :: PREC = nf90_float

  INTEGER :: dimids1d(2)
  INTEGER :: partdimid, tdimid
  INTEGER :: partid, tid
  INTEGER :: start1d(2), cnt1d(2)
  INTEGER :: npart
  INTEGER :: specid(6)
  INTEGER, ALLOCATABLE :: npartdata(:)

CONTAINS

  !*****************************************************************************

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
       WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE nf

  !*****************************************************************************

  SUBROUTINE open_trj_nc_file (ncid, filename, species, units,npartin)

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN) :: filename, species(:), units(:)
    INTEGER, INTENT(OUT) :: ncid
    INTEGER, INTENT(IN) :: npartin

    INTEGER :: i


    npart=npartin

    ALLOCATE(npartdata(npart))

    DO I=1,npart
    npartdata(I)=I
    END DO
    ! open the netcdf file (nf90_clobber = overwrite existing file)
    CALL nf(nf90_create(filename//'.nc', nf90_clobber, ncid))

    ! global attributes
    CALL nf(nf90_put_att(ncid, nf90_global, 'title', 'trj'))

    ! definition of the dimensions
    ! syntax: nf90_def_dim(IN:ncid, IN:name, IN:len, OUT:dimid)
    CALL nf(nf90_def_dim(ncid, 'particle',  npart, partdimid))
    CALL nf(nf90_def_dim(ncid, 'time', nf90_unlimited, tdimid))

!    dimids2d(:) = (/ londimid, latdimid,           tdimid /) ! 2d (3rd dim=t)
    dimids1d(:) = (/ partdimid,tdimid /) ! 3d (4th dim=t)
    cnt1d(:)    = (/ npart,1 /)

    ! definition of variables
    ! syntax: nf90_def_var(IN:ncid, IN:name, IN:xtype, IN:dimids, OUT:varid)
    ! coordinate variables
    CALL nf(nf90_def_var(ncid, 'particle',nf90_int, partdimid,  partid))
    CALL nf(nf90_def_var(ncid, 'time',  nf90_double, tdimid,    tid))

    DO i = 1, SIZE(species)
      CALL nf(nf90_def_var(ncid, TRIM(species(i)), PREC, dimids1d, specid(i)))
      CALL nf(nf90_put_att(ncid, specid(i), 'long_name', TRIM(species(i))))
      CALL nf(nf90_put_att(ncid, specid(i), 'units',     TRIM(units(i))))
    END DO

    ! assign attributes
    ! syntax: nf90_put_att(IN:ncid, IN:vid, IN:name, IN:values)
    ! longitude
    CALL nf(nf90_put_att(ncid, partid,  'long_name', 'particle'))
    CALL nf(nf90_put_att(ncid, partid,  'units',     'nro'))

    ! time
    CALL nf(nf90_put_att(ncid, tid,    'long_name', 'time'))
    CALL nf(nf90_put_att(ncid, tid,    'units',     'hours since 1900-01-01 00:00:00'))

    ! end of the definitions, switch to data mode
    CALL nf(nf90_enddef(ncid))

    ! syntax: nf90_put_var(IN:ncid, IN:varid, IN:values)
    ! write the data of the grid
    CALL nf(nf90_put_var(ncid, partid,   npartdata))

  END SUBROUTINE open_trj_nc_file

  !*****************************************************************************


  SUBROUTINE write_trj_nc_file(ncid, time, varname, var)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    REAL, INTENT(in) :: time
    CHARACTER(*), INTENT(IN) :: varname(:)
    REAL, INTENT(IN) :: var(:)
    REAL var1(SIZE(var)/SIZE(varname))
    INTEGER varid, nvar,ndat
    INTEGER :: I,J, timestep

    ! write timestep
    CALL nf(nf90_inquire_dimension(ncid, tid, len=timestep))
    timestep = timestep + 1
!    WRITE(*,*) 'timestep',timestep
    ! syntax: nf90_put_var(ncid, varid, values, start, cnt)
    ! start:  start in netcdf variable
    ! cnt:    number of netcdf variable points
    ! values: starting point of the fortran variable
    start1d = (/ 1,  timestep /)

    CALL nf(nf90_put_var(ncid, tid, time, (/timestep/) ))
    nvar=SIZE(varname)
    ndat=SIZE(var)
    DO I=1,nvar
      DO J=1,ndat/nvar
          var1(J)=var((I-1)*ndat/nvar+J)
       END DO
       CALL nf(nf90_inq_varid(ncid,varname(I),varid))
       CALL nf(nf90_put_var(ncid,varid, (/var1/), start1d, cnt1d))
    END DO

    CALL nf(nf90_sync(ncid)) ! write buffer to file

  END SUBROUTINE write_trj_nc_file



  SUBROUTINE write_trj_nc_file_old (ncid, time, varname, var1,var2,var3,var4)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    REAL, INTENT(in) :: time
    CHARACTER(*), INTENT(IN) :: varname(:)
    REAL, INTENT(in) :: var1(:),var2(:),var3(:),var4(:)
    INTEGER varid
    INTEGER :: i, timestep

    ! write timestep
    CALL nf(nf90_inquire_dimension(ncid, tid, len=timestep))
    timestep = timestep + 1
!    WRITE(*,*) 'timestep',timestep
    ! syntax: nf90_put_var(ncid, varid, values, start, cnt)
    ! start:  start in netcdf variable
    ! cnt:    number of netcdf variable points
    ! values: starting point of the fortran variable
    start1d = (/ 1,  timestep /)

    CALL nf(nf90_put_var(ncid, tid, time, (/timestep/) ))

    CALL nf(nf90_inq_varid(ncid,varname(1),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var1/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname(2),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var2/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname(3),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var3/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname(4),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var4/), start1d, cnt1d))

    CALL nf(nf90_sync(ncid)) ! write buffer to file

  END SUBROUTINE write_trj_nc_file_old

  !*****************************************************************************

  SUBROUTINE close_trj_nc_file (ncid)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    DEALLOCATE(npartdata)
    CALL nf(nf90_close(ncid))

  END SUBROUTINE close_trj_nc_file

  !*****************************************************************************

END MODULE trj_netcdf_w
!*******************************************************************************

!///////////////////////////////////////////////////////////////////////////////

!*******************************************************************************
!! trj_netcdf_w_4d, write four dimensional data to netcdf file
MODULE trj_netcdf_w_4d
  ! base in the module mo_netcdf.f90 de echam5
  USE netcdf
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: open_trj_nc_file_4d, write_trj_nc_file_4d, close_trj_nc_file_4d

  ! choose a precision:
  INTEGER, PARAMETER :: PREC = nf90_float
  ! INTEGER, PARAMETER :: PREC = nf90_double

  INTEGER :: dimids1d(2)
  INTEGER :: partdimid, tdimid
  INTEGER :: partid, tid
  INTEGER :: start1d(2), cnt1d(2)
  INTEGER :: npart
  INTEGER :: specid(4)
  !INTEGER :: npartdata(1225)
  INTEGER, ALLOCATABLE :: npartdata(:)

CONTAINS

  !*****************************************************************************

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
       WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE nf

  !*****************************************************************************

  SUBROUTINE open_trj_nc_file_4d(ncid, filename,&
    specie1,specie2,specie3,specie4, &
    units1,units2,units3,units4,npartin)

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN) :: filename, specie1,specie2,specie3,specie4, &
         units1,units2,units3,units4
    INTEGER, INTENT(OUT) :: ncid
    INTEGER, INTENT(IN) :: npartin

    INTEGER :: i

    ! define the grid size
    npart=npartin

    ALLOCATE(npartdata(npart))

    ! define the grid
    DO I=1,npart
    npartdata(I)=I
    END DO
    ! open the netcdf file (nf90_clobber = overwrite existing file)
    CALL nf(nf90_create(filename//'.nc', nf90_clobber, ncid))

    ! global attributes
    CALL nf(nf90_put_att(ncid, nf90_global, 'title', 'trj'))

    ! definition of the dimensions
    ! syntax: nf90_def_dim(IN:ncid, IN:name, IN:len, OUT:dimid)
    CALL nf(nf90_def_dim(ncid, 'particle',  npart, partdimid))
    CALL nf(nf90_def_dim(ncid, 'time', nf90_unlimited, tdimid))

!    dimids2d(:) = (/ londimid, latdimid,           tdimid /) ! 2d (3rd dim=t)
    dimids1d(:) = (/ partdimid,tdimid /) ! 3d (4th dim=t)
    cnt1d(:)    = (/ npart,1 /)

    ! definition of variables
    ! syntax: nf90_def_var(IN:ncid, IN:name, IN:xtype, IN:dimids, OUT:varid)
    ! coordinate variables
    CALL nf(nf90_def_var(ncid, 'particle',nf90_int, partdimid,  partid))
    CALL nf(nf90_def_var(ncid, 'time',  nf90_double, tdimid,    tid))

   !**********************************************************************
    CALL nf(nf90_def_var(ncid, TRIM(specie1), PREC, dimids1d, specid(1)))
    CALL nf(nf90_put_att(ncid, specid(1), 'long_name', TRIM(specie1)))
    CALL nf(nf90_put_att(ncid, specid(1), 'units',     TRIM(units1)))

    CALL nf(nf90_def_var(ncid, TRIM(specie2), PREC, dimids1d, specid(2)))
    CALL nf(nf90_put_att(ncid, specid(2), 'long_name', TRIM(specie2)))
    CALL nf(nf90_put_att(ncid, specid(2), 'units',     TRIM(units2)))

    CALL nf(nf90_def_var(ncid, TRIM(specie3), PREC, dimids1d, specid(3)))
    CALL nf(nf90_put_att(ncid, specid(3), 'long_name', TRIM(specie3)))
    CALL nf(nf90_put_att(ncid, specid(3), 'units',     TRIM(units3)))

    CALL nf(nf90_def_var(ncid, TRIM(specie4), PREC, dimids1d, specid(4)))
    CALL nf(nf90_put_att(ncid, specid(4), 'long_name', TRIM(specie4)))
    CALL nf(nf90_put_att(ncid, specid(4), 'units',     TRIM(units4)))
   !*********************************************************************

    ! assign attributes
    ! syntax: nf90_put_att(IN:ncid, IN:vid, IN:name, IN:values)
    ! longitude
    CALL nf(nf90_put_att(ncid, partid,  'long_name', 'particle'))
    CALL nf(nf90_put_att(ncid, partid,  'units',     'nro'))

    ! time
    CALL nf(nf90_put_att(ncid, tid,    'long_name', 'time'))
    CALL nf(nf90_put_att(ncid, tid,    'units',     'hours since 1900-01-01 00:00:00'))

    ! end of the definitions, switch to data mode
    CALL nf(nf90_enddef(ncid))

    ! syntax: nf90_put_var(IN:ncid, IN:varid, IN:values)
    ! write the data of the grid
    CALL nf(nf90_put_var(ncid, partid,   npartdata))

  END SUBROUTINE open_trj_nc_file_4d

  !*****************************************************************************

  SUBROUTINE write_trj_nc_file_4d(ncid, time, varname1,varname2,&
       varname3,varname4, &
       var1,var2,var3,var4)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    REAL, INTENT(in) :: time
    CHARACTER(*), INTENT(IN) :: varname1,varname2,varname3,varname4
    REAL, INTENT(in) :: var1(:),var2(:),var3(:),var4(:)
    INTEGER varid
    INTEGER :: i, timestep

    ! write timestep
    CALL nf(nf90_inquire_dimension(ncid, tid, len=timestep))
    timestep = timestep + 1
    !    WRITE(*,*) 'timestep',timestep
    ! syntax: nf90_put_var(ncid, varid, values, start, cnt)
    ! start:  start in netcdf variable
    ! cnt:    number of netcdf variable points
    ! values: starting point of the fortran variable
    start1d = (/ 1,  timestep /)

    CALL nf(nf90_put_var(ncid, tid, time, (/timestep/) ))

    CALL nf(nf90_inq_varid(ncid,varname1,varid))
    CALL nf(nf90_put_var(ncid,varid, (/var1/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname2,varid))
    CALL nf(nf90_put_var(ncid,varid, (/var2/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname3,varid))
    CALL nf(nf90_put_var(ncid,varid, (/var3/), start1d, cnt1d))
    CALL nf(nf90_inq_varid(ncid,varname4,varid))
    CALL nf(nf90_put_var(ncid,varid, (/var4/), start1d, cnt1d))

    CALL nf(nf90_sync(ncid)) ! write buffer to file

  END SUBROUTINE write_trj_nc_file_4d

  !*****************************************************************************

  SUBROUTINE close_trj_nc_file_4d (ncid)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    DEALLOCATE(npartdata)
    CALL nf(nf90_close(ncid))

  END SUBROUTINE close_trj_nc_file_4d

  !*****************************************************************************

END MODULE trj_netcdf_w_4d

!*******************************************************************************

!///////////////////////////////////////////////////////////////////////////////

!*******************************************************************************
!! In process
MODULE trj_netcdf_w_2d
  ! base in the module mo_netcdf.f90 de echam5
  USE netcdf
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: open_trj_nc_file, write_trj_nc_file, close_trj_nc_file

  ! choose a precision:
  INTEGER, PARAMETER :: PREC = nf90_float
  ! INTEGER, PARAMETER :: PREC = nf90_double

  INTEGER :: dimids2d(3)
  INTEGER :: longdimid,latdimid, tdimid
  INTEGER :: longid,latid, tid
  INTEGER :: start2d(3), cnt2d(3)
  INTEGER :: npartx,nparty
  INTEGER :: specid(4)
  INTEGER :: npartdata(140,70)
CONTAINS

  !*****************************************************************************

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
       WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE nf

  !*****************************************************************************

  SUBROUTINE open_trj_nc_file (ncid, filename, species, units,xdata,ydata)

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN) :: filename, species(:), units(:)
    REAL, INTENT(IN) :: xdata(:), ydata(:)
    INTEGER, INTENT(OUT) :: ncid

    INTEGER :: ix, iy, i

    ! define the grid size
    !npart=1225
    npartx=140
    nparty=70

    ! define the grid
        ! open the netcdf file (nf90_clobber = overwrite existing file)
    CALL nf(nf90_create(filename//'.nc', nf90_clobber, ncid))

    ! global attributes
    CALL nf(nf90_put_att(ncid, nf90_global, 'title', 'trj'))

    ! definition of the dimensions
    ! syntax: nf90_def_dim(IN:ncid, IN:name, IN:len, OUT:dimid)
    CALL nf(nf90_def_dim(ncid, 'longitude',  npartx, longdimid))
    CALL nf(nf90_def_dim(ncid, 'latitude',  nparty, latdimid))
    CALL nf(nf90_def_dim(ncid, 'time', nf90_unlimited, tdimid))

!    dimids2d(:) = (/ londimid, latdimid,           tdimid /) ! 2d (3rd dim=t)
    dimids2d(:) = (/ longdimid,latdimid,tdimid /) ! 3d (4th dim=t)
    cnt2d(:)    = (/ npartx,nparty,1 /)

    ! definition of variables
    ! syntax: nf90_def_var(IN:ncid, IN:name, IN:xtype, IN:dimids, OUT:varid)
    ! coordinate variables
    CALL nf(nf90_def_var(ncid, 'longitude',nf90_int, longdimid,  longid))
    CALL nf(nf90_def_var(ncid, 'latitude',nf90_int, latdimid,  latid))
    CALL nf(nf90_def_var(ncid, 'time',  nf90_double, tdimid,    tid))

    DO i = 1, SIZE(species)
      CALL nf(nf90_def_var(ncid, TRIM(species(i)), PREC, dimids2d, specid(i)))
      CALL nf(nf90_put_att(ncid, specid(i), 'long_name', TRIM(species(i))))
      CALL nf(nf90_put_att(ncid, specid(i), 'units',     TRIM(units(i))))
    END DO

    ! assign attributes
    ! syntax: nf90_put_att(IN:ncid, IN:vid, IN:name, IN:values)
    ! longitude
    CALL nf(nf90_put_att(ncid, longid,  'long_name', 'longitude'))
    CALL nf(nf90_put_att(ncid, longid,  'units',     'degrees'))

    CALL nf(nf90_put_att(ncid, latid,  'long_name', 'latitude'))
    CALL nf(nf90_put_att(ncid, latid,  'units',     'degrees'))

    ! time
    CALL nf(nf90_put_att(ncid, tid,    'long_name', 'time'))
    CALL nf(nf90_put_att(ncid, tid,    'units',     'hours since 1900-01-01 00:00:00'))

    ! end of the definitions, switch to data mode
    CALL nf(nf90_enddef(ncid))

    ! syntax: nf90_put_var(IN:ncid, IN:varid, IN:values)
    ! write the data of the grid
    CALL nf(nf90_put_var(ncid, longid,   xdata))
    CALL nf(nf90_put_var(ncid, latid,   ydata))

  END SUBROUTINE open_trj_nc_file

  !*****************************************************************************

  SUBROUTINE write_trj_nc_file (ncid, time, varname, var1,var2,var3,var4)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid
    REAL, INTENT(in) :: time
    CHARACTER(*), INTENT(IN) :: varname(:)
    REAL, INTENT(in) :: var1(:,:),var2(:,:),var3(:,:),var4(:,:)
    INTEGER varid
    INTEGER :: i, timestep

    ! write timestep
    CALL nf(nf90_inquire_dimension(ncid, tid, len=timestep))
    timestep = timestep + 1
!    WRITE(*,*) 'timestep',timestep
    ! syntax: nf90_put_var(ncid, varid, values, start, cnt)
    ! start:  start in netcdf variable
    ! cnt:    number of netcdf variable points
    ! values: starting point of the fortran variable
    start2d = (/ 1, 1, timestep /)

    CALL nf(nf90_put_var(ncid, tid, time, (/timestep/) ))

    CALL nf(nf90_inq_varid(ncid,varname(1),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var1/), start2d, cnt2d))
    CALL nf(nf90_inq_varid(ncid,varname(2),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var2/), start2d, cnt2d))
    CALL nf(nf90_inq_varid(ncid,varname(3),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var3/), start2d, cnt2d))
    CALL nf(nf90_inq_varid(ncid,varname(4),varid))
    CALL nf(nf90_put_var(ncid,varid, (/var4/), start2d, cnt2d))

    CALL nf(nf90_sync(ncid)) ! write buffer to file

  END SUBROUTINE write_trj_nc_file

  !*****************************************************************************

  SUBROUTINE close_trj_nc_file (ncid)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid

    CALL nf(nf90_close(ncid))

  END SUBROUTINE close_trj_nc_file

  !*****************************************************************************

END MODULE trj_netcdf_w_2d

!*******************************************************************************
