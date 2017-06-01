MODULE parametros
  IMPLICIT none
  REAL Rt,DegToR,Pi
  
  parameter (Rt=6.371229E+06)
  parameter (Pi=3.14159265358979)
  parameter (DegToR=0.01745329251994)
  
END MODULE parametros

! lon, lat, grilla uniforme en latitud longitud
! xstr, ystr grilla uniforme en coordenadas stereograficas
! xgeo,ygeo grilla uniforme latitud longitud en coordenadas stereograficas
! lonstr, latstr grilla uniforme stereografica en cordenadas geograficas
MODULE strgrid
  USE parametros
  IMPLICIT none
  REAL, ALLOCATABLE :: lon(:),lat(:),xgeo(:,:),ygeo(:,:),var(:,:)
  REAL, ALLOCATABLE :: xstr(:),ystr(:),lonstr(:,:),latstr(:,:),varstr(:,:)
  INTEGER :: nx,ny,nlon,nlat
 
  PUBLIC :: angrid, strcgrid,strsave

CONTAINS

  SUBROUTINE angrid(ilon,dlon,nloni,ilat,dlat,nlati)
    INTEGER, INTENT(IN) ::nloni,nlati
    REAL, INTENT(IN) :: ilon,dlon,ilat,dlat
    
    INTEGER :: ix, iy
    nlon=nloni; nlat=nlati;

    ALLOCATE(lon(nlon),lat(nlat),var(nlon,nlat),xgeo(nlon,nlat),ygeo(nlon,nlat))
    
    DO ix=1,nlon
       lon(ix)=ilon+(ix-1)*dlon
    END DO   
    DO iy=1,nlat
       lat(iy)=ilat+(iy-1)*dlat
    END DO

    DO ix=1,nlon
       DO iy=1,nlat
          var(:,:)=sin(Pi*lon(ix)/180.)*cos(3*Pi*lat(iy)/180.)*sin(Pi*lat(iy)/180.)
       END DO
    END DO

    DO ix=1,nlon
       DO iy=1,nlat
          CALL ang_to_str(lon(ix)*Pi/180.,lat(iy)*Pi/180.,xgeo(ix,iy),ygeo(ix,iy))
       END DO
    END DO

  END SUBROUTINE angrid  
  
  SUBROUTINE strcgrid(ixstr,dx,nxstr,iystr,dy,nystr)
    IMPLICIT none
    INTEGER, INTENT(IN) :: nxstr,nystr
    REAL, INTENT(IN) :: ixstr,dx,iystr,dy
    INTEGER ix,iy
    
    nx=nxstr; ny=nystr;

    ALLOCATE(xstr(nx),ystr(ny),varstr(nx,ny),lonstr(nx,ny),latstr(nx,ny))

    DO ix=1,nx
       xstr(ix)=ixstr+dx*(ix-1)
    END DO   

    DO iy=1,ny
       ystr(iy)=iystr+dy*(iy-1)
    END DO   

    DO ix=1,nx
       DO iy=1,ny
          CALL str_to_ang(lonstr(ix,iy),latstr(ix,iy),xstr(ix),ystr(iy))
          lonstr(ix,iy)=lonstr(ix,iy)*180./Pi
          latstr(ix,iy)=latstr(ix,iy)*180./Pi
          WRITE(*,*)lonstr(ix,iy), latstr(ix,iy)
       END DO
    END DO

  END SUBROUTINE strcgrid  

  SUBROUTINE strintp
    IMPLICIT none
  END SUBROUTINE strintp

  SUBROUTINE strsave(Filegeo,Filestr)
    IMPLICIT none
    CHARACTER(*) Filegeo, Filestr

    INTEGER :: ix, iy

    OPEN(22,FILE=Filegeo)
    DO ix=1,nlon
       DO iy=1,nlat
          WRITE(22,FMT='(5F13.5)')lon(ix),lat(iy),xgeo(ix,iy),ygeo(ix,iy),var(ix,iy)
       END DO
    END DO   
    CLOSE(22)
    OPEN(22,FILE=Filestr)
    DO ix=1,nx
       DO iy=1,ny
          WRITE(22,FMT='(5F13.5)')xstr(ix),ystr(iy),lonstr(ix,iy),latstr(ix,iy),varstr(ix,iy)
       END DO
    END DO   
    CLOSE(22)


  END SUBROUTINE strsave

END MODULE strgrid

PROGRAM strgridmain
  USE strgrid
  IMPLICIT none

  CALL angrid(0.,1.,361,-90.,1.,100)
  CALL strcgrid(-0.5,0.01,101,-0.5,0.01,101) 
  CALL strsave('../output/geodata.dat','../output/strdata.dat')

END PROGRAM strgridmain
