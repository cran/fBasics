
C This library is free software; you can redistribute it and/or
C modify it under the terms of the GNU Library General Public
C License as published by the Free Software Foundation; either
C version 2 of the License, or (at your option) any later version.
C
C This library is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
C GNU Library General Public License for more details.
C
C You should have received a copy of the GNU Library General 
C Public License along with this library; if not, write to the 
C Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
C MA  02111-1307  USA


C
C Bin-Zhou's Devolatilization Procedure
C
C Subroutine written by Diethelm Wuertz
C www.rmetrics.org
C
C  s(nt)  original time series of log prices
C  ms(nt) index
C  nt     their lengths
C  k      length of averaging interval
C  vmax   volatility threshold 

      subroutine dv(vmax, s, ms, nt, ns, k)
C
C>>> DECLARATIONS:
      integer nt, ms(nt), k, ns, m, j
      real*8 v, vmax, s(nt)
      real*8 s1, s2, ok, tks
C
C>>> SETTINGS:
      ns=0
      m=k
      ok=1.0d0/real(k)
      tks=2.0d0/real(k*k)
      do i=1, nt
         ms(i)=-1
      enddo
C
C>>> DEVOLATILIZATION:
 11   continue
      v=0.0d0
      do i=m+1, nt
         v=v+ok*(s(i-k)-s(i))**2
         do j=0, k-1
            s1=s(i-j-1)-s(i-j)
            s2=s(i-j-2)-s(i-j-1)
            v=v+tks*s1*s2
         enddo
         if (v.ge.vmax) then
            ns=ns+1
            m=i
            if (ns.lt.nt) ms(i)=+1
            if(m.lt.nt) goto 11
         endif
      enddo
C
      return
      end