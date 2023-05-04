      subroutine vdload (
C Read only (unmodifiable)variables -
     1 nblock, ndim, stepTime, totalTime,
     2 amplitude, curCoords, velocity, dirCos, jltyp, sname,
C Write only (modifiable) variable -
     1 value )
C
      include 'vaba_param.inc'
C
      dimension curCoords(nblock,ndim), velocity(nblock,ndim),
     1  dirCos(nblock,ndim,ndim), value(nblock)
      character*80 sname
C
	   !dimensions of the tyre , length and width respectively
		real*8 a,b
	   !dimensions of the vehicle's axes , longitudinal and vertical respectively
		real*8 LA,LB
	   !tyre pressure
		real*8 p
		
		a=0.35
		b=0.2
		LA=5
		LB=2.5
		p=1000
		slabWidth=10
		vehicleVelocity=20
		displacement=vehicleVelocity*stepTime

      do km = 1, nblock
		! in width
        if ( curCoords(km,1)<(slabWidth-LB)/2. .and. curCoords(km,1)>(slabWidth-LB)/2.-b) then
		
		! in longitudinal direction
			if ( curCoords(km,3)<displacement+a .and. curCoords(km,3)>displacement) then
				value (km)=p
			else if ( curCoords(km,3)<displacement+a+LA .and. curCoords(km,3)>displacement+LA) then
				value (km)=p
			else 
				value (km)=0.0
			end if
			
		else if ( curCoords(km,1)>(slabWidth+LB)/2. .and. curCoords(km,1)<(slabWidth+LB)/2.+b) then
			! in longitudinal direction
			if ( curCoords(km,3)<displacement+a .and. curCoords(km,3)>displacement) then
				value (km)=p
			else if ( curCoords(km,3)<displacement+a+LA .and. curCoords(km,3)>displacement+LA) then
				value (km)=p
			else
				value (km)=0.0
			end if
		
		else
		
		value (km)=0.0
		
		end if
	  end do


      return
      end