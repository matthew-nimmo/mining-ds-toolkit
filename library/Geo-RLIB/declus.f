c-----------------------------------------------------------------------
c Modified from GSLIB
c Copyright (C) 1996, The Board of Trustees of the Leland Stanford
c See paper in Computers and Geosciences Vol 15 No 3 (1989) pp 325-332
c
c DECLUS: a three dimensional cell declustering program
c *****************************************************
c INPUT/OUTPUT Parameters:
c   yanis,zanis     Y and Z cell anisotropy (Ysize=size*Yanis)
c   minmax         0=look for minimum declustered mean (1=max)
c   ncell,cmin,cmax number of cell sizes, min size, max size
c   noff            number of origin offsets
c-----------------------------------------------------------------------
      subroutine  declus(nd, nc, vr, x, y, z, wtopt, yanis, zanis,
     +            ncell, cmin, cmax, noff, minmax)
      integer     i, lp, kp, nd, nc, ncell, minmax, noff, ijk(nd),
     +            ncellx, ncelly, ncellz, ncellt,
     +            icell, icellx, icelly, icellz, ipoint
      real*8      vr(nd), x(nd), y(nd), z(nd), wtopt(nd),
     +            yanis, zanis, cmin, cmax, wt(nd), wtmin, wtmax,
     +            sumw, sumwg, xcs, ycs, zcs, xo1, yo1, zo1, vrav, vrop,
     +            best, xmin, xmax, ymin, ymax, zmin, zmax, roff, facto,
     +            xinc, yinc, zinc, xfac, yfac, zfac, xo, yo, zo,
     +            vrmin, vrmax, vrcr, cellwt(nc)
      logical     imin
      data        xmin/ 1.0e21/,ymin/ 1.0e21/,zmin/ 1.0e21/,
     +            xmax/-1.0e21/,ymax/-1.0e21/,zmax/-1.0e21/,
     +            vrmin/ 1.0e21/,vrmax/-1.0e21/
c
      imin  = .true.
      if(minmax.eq.1) imin = .false.
      roff = real(noff)
      write(*,800) minmax,yanis,zanis,ncell,cmin,cmax,noff
 800  format(/' minmax  =',i8,/,
     +        ' yanis   = ',f12.5,/,
     +        ' zanis   = ',f12.5,/,
     +        ' ncell   = ',i8,/,
     +        ' cmin    = ',f12.5,/,
     +        ' cmax    = ',f12.5,/,
     +        ' noff   = ',i8,/)
c
c compute min, max, and average:
c
      vrav = 0.0
      do i=1,nd
         wtopt(i) = 1.0
         vrav  = vrav + vr(i)
         if(vr(i).lt.vrmin) vrmin=vr(i)
         if(vr(i).gt.vrmax) vrmax=vr(i)
         if(x(i).lt.xmin) xmin=x(i)
         if(x(i).gt.xmax) xmax=x(i)
         if(y(i).lt.ymin) ymin=y(i)
         if(y(i).gt.ymax) ymax=y(i)
         if(z(i).lt.zmin) zmin=z(i)
         if(z(i).gt.zmax) zmax=z(i)
      end do
      vrav = vrav / real(nd)
c
c      if((zmax-zmin).le.0) then
c            z(nd) = 0.0
c      endif
c
c Write Some of the Statistics to the screen:
c
      write(*,900) nd,vrav,vrmin,vrmax,(xmax-xmin),
     +             (ymax-ymin),(zmax-zmin)
 900  format(/' There are ',i8,' data with:',/,
     +        '   mean value            = ',f12.5,/,
     +        '   minimum and maximum   = ',2f12.5,/,
     +        '   size of data vol in X = ',f12.5,/,
     +        '   size of data vol in Y = ',f12.5,/,
     +        '   size of data vol in Z = ',f12.5,/)
c
c initialize the "best" weight values:
c
      vrop = vrav
      best = 0.0
c
c define a "lower" origin to use for the cell sizes:
c
      xo1 = xmin - 0.01
      yo1 = ymin - 0.01
      zo1 = zmin - 0.01
c
c define the increment for the cell size:
c
      xinc = (cmax-cmin) / real(ncell)
      yinc = yanis * xinc
      zinc = zanis * xinc
c
c loop over "ncell+1" cell sizes in the grid network:
c
      xcs = cmin - xinc
      ycs = (cmin*yanis) - yinc
      zcs = (cmin*zanis) - zinc
c
c MAIN LOOP over cell sizes:
c
      do lp=1,ncell+1
         xcs = xcs + xinc
         ycs = ycs + yinc
         zcs = zcs + zinc
c
c initialize the weights to zero:
c
         do i=1,nd
            wt(i) = 0.0
         end do
c
c determine the maximum number of grid cells in the network:
c
         ncellx = int((xmax-(xo1-xcs))/xcs)+1
         ncelly = int((ymax-(yo1-ycs))/ycs)+1
         ncellz = int((zmax-(zo1-zcs))/zcs)+1
         ncellt = (ncellx*ncelly*ncellz)
c
c loop over all the origin offsets selected:
c
         xfac = amin1((xcs/roff),(0.5*(xmax-xmin)))
         yfac = amin1((ycs/roff),(0.5*(ymax-ymin)))
         zfac = amin1((zcs/roff),(0.5*(zmax-zmin)))
         do kp=1,noff
            xo = xo1 - (real(kp)-1.0)*xfac
            yo = yo1 - (real(kp)-1.0)*yfac
            zo = zo1 - (real(kp)-1.0)*zfac
c
c initialize the cumulative weight indicators:
c
            do i=1,ncellt
               cellwt(i) = 0.0
            end do
c
c determine which cell each datum is in:
c
            do i=1,nd
               icellx = int((x(i) - xo)/xcs) + 1
               icelly = int((y(i) - yo)/ycs) + 1
               icellz = int((z(i) - zo)/zcs) + 1
               icell = icellx + (icelly-1)*ncellx  
     +                  + (icellz-1)*ncelly*ncellx
               ijk(i) = icell
               cellwt(icell) = cellwt(icell) + 1.0
            end do
c
c The weight assigned to each datum is inversely proportional to the
c number of data in the cell.  We first need to get the sum of weights
c so that we can normalize the weights to sum to one:
c
            sumw = 0.0
            do i=1,nd
               ipoint = ijk(i)
               sumw = sumw + (1.0 / cellwt(ipoint))
            end do
            sumw = 1.0 / sumw
c
c Accumulate the array of weights (that now sum to one):
c
            do i=1,nd
               ipoint = ijk(i)
               wt(i) = wt(i) + (1.0/cellwt(ipoint))*sumw
            end do
c
c End loop over all offsets:
c
         end do
c
c compute the weighted average for this cell size:
c
         sumw  = 0.0
         sumwg = 0.0
         do i=1,nd
            sumw  = sumw  + wt(i)
            sumwg = sumwg + wt(i)*vr(i)
         end do
         vrcr  = sumwg / sumw
c
c see if this weighting is optimal:
c
         if((imin.and.vrcr.lt.vrop).or.
     +   (.not.imin.and.vrcr.gt.vrop).or.(ncell.eq.1)) then
            best = xcs
            vrop = vrcr
            do i=1,nd
               wtopt(i) = wt(i)
            end do
         endif
c
c END MAIN LOOP over all cell sizes:
c
      end do
c
c Get the optimal weights:
c
      sumw = 0.0
      do i=1,nd
         sumw = sumw + wtopt(i)
      end do
      wtmin = 99999.0
      wtmax = -99999.0
      facto = real(nd) / sumw
      do i = 1,nd
         wtopt(i) = wtopt(i) * facto
         if(wtopt(i).lt.wtmin) wtmin = wtopt(i)
         if(wtopt(i).gt.wtmax) wtmax = wtopt(i)
      end do
c
c Some Debugging Information:
c
      write(*,901) vrop,wtmin,wtmax,1.0
 901  format('   declustered mean      = ',f12.5,/,
     +       '   min and max weight    = ',2f12.5,/,
     +       '   equal weighting       = ',f12.5,/)
c
c Finished:
c
      return
      end
