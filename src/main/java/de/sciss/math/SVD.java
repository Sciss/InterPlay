/*
 *  SVD.scala
 *  (de.sciss.math package)
 *
 *  Copyright (c) 2001-2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.math;

public class SVD {
    private SVD() {}

	// more ore less directly taken from JAMA
	// (http://math.nist.gov/javanumerics/jama)
	// which is released in the public domain.
	public static void svd( float[][] mat, float[] s, float[][] u, float[][] v ) {
		// Derived from LINPACK code.
		// Initialize.
		final int m		= mat.length;
		final int n		= mat[ 0 ].length;
		final int nu	= Math.min( m, n );
		final int ns	= Math.min( m + 1, n );

		final boolean wantu = u != null;
		final boolean wantv = v != null;

		if( (s.length != ns) ||
		    (wantu && ((u.length != m) || (u[ 0 ].length != nu))) ||
		    (wantv && ((v.length != n) || (v[ 0 ].length != n))) ) throw new IllegalArgumentException();

		// Apparently the failing cases are only a proper subset of (m<n),
		// so let's not throw error.  Correct fix to come later?
		// if (m<n) {
		// throw new IllegalArgumentException("Jama SVD only works for m >= n"); }

		// final float[] s = new float [Math.min(m+1,n)];
		// final float[][] u = new float [m][nu];
		// final float[][] v = new float [n][n];

		final float[]	e		= new float[ n ];
		final float[]	work	= new float[ m ];
		final float	eps		= (float) Math.pow( 2.0, -48.0 );	// -52
		final float	tiny	= (float) Math.pow( 2.0, -120.0 );	// -966

		// Reduce A to bidiagonal form, storing the diagonal elements
		// in s and the super-diagonal elements in e.

		final int nct = Math.min( m - 1, n );
		final int nrt = Math.max( 0, Math.min( n - 2, m ));
		final int nk  = Math.max( nct, nrt );
		final int np  = Math.min( n, m + 1 );

		for( int k = 0; k < nk; k++ ) {
			if( k < nct ) {
				// Compute the transformation for the k-th column and
				// place the k-th diagonal in s[k].
				// Compute 2-norm of k-th column without under/overflow.
				s[ k ] = 0;
				for( int i = k; i < m; i++ ) {
					s[ k ] = hypot( s[ k ], mat[ i ][ k ]);
				}
				if( s[ k ] != 0.0f ) {
					if( mat[ k ][ k ] < 0.0f ) {
						s[ k ] = -s[ k ];
					}
					for( int i = k; i < m; i++ ) {
						mat[ i ][ k ] /= s[ k ];
					}
					mat[ k ][ k ] += 1.0f;
				}
				s[ k ] = -s[ k ];
			}

			for( int j = k + 1; j < n; j++ ) {
				if( (k < nct) && (s[ k ] != 0.0f) ) {
					// Apply the transformation.
					float t = 0;
					for( int i = k; i < m; i++ ) {
						t += mat[ i ][ k ] * mat[ i ][ j ];
					}
					t = -t / mat[ k ][ k ];
					for( int i = k; i < m; i++ ) {
						mat[ i ][ j ] += t * mat[ i ][ k ];
					}
				}
				// Place the k-th row of A into e for the
				// subsequent calculation of the row transformation.
				e[ j ] = mat[ k ][ j ];
			}

			if( wantu && (k < nct) ) {
				// Place the transformation in U for subsequent back
				// multiplication.
				for( int i = k; i < m; i++ ) {
					u[ i ][ k ] = mat[ i ][ k ];
				}
			}

			if( k < nrt ) {
				// Compute the k-th row transformation and place the
				// k-th super-diagonal in e[k].
				// Compute 2-norm without under/overflow.
				e[ k ] = 0;
				for( int i = k + 1; i < n; i++ ) {
					e[ k ] = hypot( e[ k ], e[ i ]);
				}
				if( e[ k ] != 0.0f ) {
					if( e[ k + 1 ] < 0.0f ) {
						e[ k ] = -e[ k ];
					}
					for( int i = k + 1; i < n; i++ ) {
						e[ i ] /= e[ k ];
					}
					e[ k + 1 ] += 1.0f;
				}
				e[ k ] = -e[ k ];
				if( ((k + 1) < m) && (e[ k ] != 0.0f) ) {
					// Apply the transformation.
					for( int i = k + 1; i < m; i++ ) {
						work[ i ] = 0.0f;
					}
					for( int j = k + 1; j < n; j++ ) {
						for( int i = k + 1; i < m; i++ ) {
							work[ i ] += e[ j ] * mat[ i ][ j ];
						}
					}
					for( int j = k + 1; j < n; j++ ) {
						final float t = -e[ j ] / e[ k + 1 ];
						for( int i = k + 1; i < m; i++ ) {
							mat[ i ][ j ] += t * work[ i ];
						}
					}
				}
				if( wantv ) {
					// Place the transformation in V for subsequent
					// back multiplication.
					for( int i = k + 1; i < n; i++ ) {
						v[ i ][ k ] = e[ i ];
					}
				}
			}
		}

		// Set up the final bidiagonal matrix or order p.
		int p = np;
		if( nct < n ) {
			s[ nct ] = mat[ nct ][ nct ];
		}
		if( m < p ) {
			s[ p - 1 ] = 0.0f;
		}
		if( (nrt + 1) < p ) {
			e[ nrt ] = mat[ nrt ][ p - 1 ];
		}
		e[ p - 1 ] = 0.0f;

		// If required, generate U.
		if( wantu ) {
			for( int j = nct; j < nu; j++ ) {
				for( int i = 0; i < m; i++ ) {
					u[ i ][ j ] = 0.0f;
				}
				u[ j ][ j ] = 1.0f;
			}
			for( int k = nct - 1; k >= 0; k-- ) {
				if( s[ k ] != 0.0f ) {
					for( int j = k + 1; j < nu; j++ ) {
						float t = 0;
						for( int i = k; i < m; i++ ) {
							t += u[ i ][ k ] * u[ i ][ j ];
						}
						t = -t / u[ k ][ k ];
						for( int i = k; i < m; i++ ) {
							u[ i ][ j ] += t * u[ i ][ k ];
						}
					}
					for( int i = k; i < m; i++ ) {
						u[ i ][ k ] = -u[ i ][ k ];
					}
					u[ k ][ k ] = 1.0f + u[ k ][ k ];
					for( int i = 0; i < k - 1; i++ ) {
						u[ i ][ k ] = 0.0f;
					}
				} else {
					for( int i = 0; i < m; i++)  {
						u[ i ][ k ] = 0.0f;
					}
					u[ k ][ k ] = 1.0f;
				}
			}
		}

		// If required, generate V.
		if( wantv ) {
			for( int k = n - 1; k >= 0; k-- ) {
				if( (k < nrt) && (e[ k ] != 0.0f) ) {
					for( int j = k + 1; j < nu; j++ ) {
						float t = 0;
						for( int i = k + 1; i < n; i++ ) {
							t += v[ i ][ k ] * v[ i ][ j ];
						}
						t = -t / v[ k + 1 ][ k ];
						for( int i = k + 1; i < n; i++ ) {
							v[ i ][ j ] += t * v[ i ][ k ];
						}
					}
				}
				for( int i = 0; i < n; i++ ) {
					v[ i ][ k ] = 0.0f;
				}
				v[ k ][ k ] = 1.0f;
			}
		}

		// Main iteration loop for the singular values.
		final int	pp		= p - 1;
		int			iter	= 0;
		while( p > 0 ) {
			int k, kase;

			// Here is where a test for too many iterations would go.

			// This section of the program inspects for
			// negligible elements in the s and e arrays.  On
			// completion the variables kase and k are set as follows.

			// kase = 1     if s(p) and e[k-1] are negligible and k<p
			// kase = 2     if s(k) is negligible and k<p
			// kase = 3     if e[k-1] is negligible, k<p, and
			//              s(k), ..., s(p) are not negligible (qr step).
			// kase = 4     if e(p-1) is negligible (convergence).

			for( k = p - 2; k >= -1; k-- ) {
				if( k == -1 ) break;
				if( Math.abs( e[ k ]) <=
					tiny + eps * (Math.abs( s[ k ]) + Math.abs( s[ k + 1 ]))) {

					e[ k ] = 0.0f;
					break;
				}
			}
			if( k == p - 2 ) {
				kase = 4;
			} else {
				int ks;
				for( ks = p - 1; ks >= k; ks-- ) {
					if( ks == k ) break;
					final float t = (ks != p ? Math.abs( e[ ks ]) : 0.0f) +
									 (ks != k + 1 ? Math.abs( e[ ks - 1 ]) : 0.0f);
					if( Math.abs( s[ ks ]) <= tiny + eps * t)  {
						s[ ks ] = 0.0f;
						break;
					}
				}
				if( ks == k ) {
					kase = 3;
				} else if( ks == p - 1 ) {
					kase = 1;
				} else {
					kase = 2;
					k = ks;
				}
			}
			k++;

			// Perform the task indicated by kase.
			switch( kase ) {

			// Deflate negligible s(p).
			case 1: {
				float f = e[ p - 2 ];
				e[ p - 2 ] = 0.0f;
				for( int j = p - 2; j >= k; j-- ) {
					final float t = hypot( s[ j ], f );
					final float cs = s[ j ] / t;
					final float sn = f / t;
					s[ j ] = t;
					if( j != k ) {
						f			= -sn * e[ j - 1 ];
						e[ j - 1 ]	=  cs * e[ j - 1 ];
					}
					if( wantv ) {
						for( int i = 0; i < n; i++ ) {
							final float tt = cs * v[ i ][ j ] + sn * v[ i ][ p - 1 ];
							v[ i ][ p - 1 ] = -sn * v[ i ][ j ] + cs * v[ i ][ p - 1 ];
							v[ i ][ j ]     = tt;
						}
					}
				}
			}
			break;

			// Split at negligible s(k).
			case 2: {
				float f = e[ k - 1 ];
				e[ k - 1 ] = 0.0f;
				for( int j = k; j < p; j++ ) {
					final float t	= hypot( s[ j ], f );
					final float cs = s[ j ] / t;
					final float sn = f / t;
					s[ j ] = t;
					f = -sn * e[ j ];
					e[ j ] = cs * e[ j ];
					if( wantu ) {
						for( int i = 0; i < m; i++ ) {
							final float tt = cs * u[ i ][ j ] + sn * u[ i ][ k - 1 ];
							u[ i ][ k - 1 ] = -sn * u[ i ][ j ] + cs * u[ i ][ k - 1 ];
							u[ i ][ j ]     = tt;
						}
					}
				}
			}
			break;

			// Perform one qr step.
			case 3: {
				// Calculate the shift.
				final float scale = Math.max( Math.max( Math.max( Math.max(
				    Math.abs( s[ p - 1 ]), Math.abs( s[ p - 2 ])), Math.abs( e[ p - 2 ])),
				    Math.abs( s[ k ])), Math.abs( e[ k ]));
				final float sp		= s[ p - 1 ] / scale;
				final float spm1	= s[ p - 2 ] / scale;
				final float epm1	= e[ p - 2 ] / scale;
				final float sk		= s[ k ] / scale;
				final float ek		= e[ k ] / scale;
				final float b		= ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0f;
				final float c		= (sp * epm1) * (sp * epm1);
				final float shift;
				if( (b != 0.0f) || (c != 0.0f) ) {
					final float t;
					if( b >= 0.0f ) {
						t = (float) Math.sqrt(b*b + c);
					} else {
						t = (float) -Math.sqrt(b*b + c);
					}
					shift = c / (b + t);
				} else {
					shift = 0.0f;
				}
				float f = (sk + sp) * (sk - sp) + shift;
				float g = sk * ek;

				// Chase zeros.
				for( int j = k; j < (p - 1); j++ ) {
					float t	= hypot( f, g );
					float cs	= f / t;
					float sn	= g / t;
					if( j != k ) {
						e[ j - 1 ] = t;
					}
					f			= cs * s[ j ] + sn * e[ j ];
					e[ j ]		= cs * e[ j ] - sn * s[ j ];
					g			= sn * s[ j + 1 ];
					s[ j + 1 ]	= cs * s[ j + 1 ];
					if( wantv ) {
						for( int i = 0; i < n; i++ ) {
							final float tt = cs * v[ i ][ j ] + sn * v[ i ][ j + 1 ];
							v[ i ][ j + 1 ] = -sn * v[ i ][ j ] + cs * v[ i ][ j + 1 ];
							v[ i ][ j ]     = tt;
						}
					}
					t			= hypot( f, g );
					cs			= f / t;
					sn			= g / t;
					s[ j ]		= t;
					f			=  cs * e[ j ] + sn * s[ j + 1 ];
					s[ j + 1 ]	= -sn * e[ j ] + cs * s[ j + 1 ];
					g			=  sn * e[ j +1 ];
					e[ j + 1 ]	=  cs * e[ j + 1 ];
					if( wantu && (j < (m - 1)) ) {
						for( int i = 0; i < m; i++ ) {
							final float tt = cs * u[ i ][ j ] + sn * u[ i ][ j + 1 ];
							u[ i ][ j + 1 ] = -sn * u[ i ][ j ] + cs * u[ i ][ j + 1 ];
							u[ i ][ j ]     = tt;
						}
					}
				}
				e[ p - 2 ] = f;
				iter++;
			}
			break;

			// Convergence.
			case 4: {
				// Make the singular values positive.
				if( s[ k ] <= 0.0f ) {
					s[ k ] = (s[ k ] < 0.0f ? -s[ k ] : 0.0f);
					if( wantv ) {
						for( int i = 0; i <= pp; i++ ) {
							v[ i ][ k ] = -v[ i ][ k ];
						}
					}
				}

				// Order the singular values.
				while( k < pp ) {
					if( s[ k ] >= s[ k + 1 ]) break;
					float t	= s[ k ];
					s[ k ]		= s[ k + 1 ];
					s[ k + 1 ]	= t;
					if( wantv && (k < (n - 1)) ) {
						for( int i = 0; i < n; i++ ) {
							t = v[ i ][ k + 1 ];
							v[ i ][ k + 1 ] = v[ i ][ k ];
							v[ i ][ k ] = t;
						}
					}
					if( wantu && (k < (m - 1)) ) {
						for( int i = 0; i < m; i++ ) {
							t = u[ i ][ k + 1 ];
							u[ i ][ k + 1 ] = u[ i ][ k ];
							u[ i ][ k ] = t;
						}
					}
					k++;
				}
				iter = 0;
				p--;
			}
			break;
			}
		} // while( p > 0 )
	}

	// sqrt(a^2 + b^2) without under/overflow.
	private static float hypot( float a, float b ) {
		if( Math.abs( a ) > Math.abs( b )) {
			final float div = b / a;
			return( (float) (Math.abs( a ) * Math.sqrt( 1 + div * div )));
		} else if( b != 0 ) {
			final float div = a / b;
			return( (float) (Math.abs( b ) * Math.sqrt( 1 + div * div )));
		} else {
			return 0.0f;
		}
	}
}
