/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.util

trait StringSimilarity{

  def getLevenshteinDistance (s:String, t:String):Int = {

    if (s == null || t == null) {
      throw new IllegalArgumentException("Strings must not be null");
    }
    
    /*
    The difference between this impl. and the previous is that, rather 
    than creating and retaining a matrix of size s.length()+1 by t.length()+1, 
    we maintain two single-dimensional arrays of length s.length()+1.  The first, d,
    is the 'current working' distance array that maintains the newest distance cost
    counts as we iterate through the characters of String s.  Each time we increment
    the index of String t we are comparing, d is copied to p, the second int[].  Doing so
    allows us to retain the previous cost counts as required by the algorithm (taking 
    the minimum of the cost count to the left, up one, and diagonally up and to the left
    of the current cost count being calculated).  (Note that the arrays aren't really 
    copied anymore, just switched...this is clearly much better than cloning an array 
    or doing a System.arraycopy() each time  through the outer loop.)

    Effectively, the difference between the two implementations is this one does not 
    cause an out of memory condition when calculating the LD over two very large strings.  		
    */		
    
    val n = s.length // length of s
    val m = t.length // length of t
    
    if (n == 0) {
      return m;
    } else if (m == 0) {
      return n;
    }

    var p = new Array[Int](n+1) //'previous' cost array, horizontally
    var d = new Array[Int](n+1) // cost array, horizontally
    var _d:Array[Int] = null //placeholder to assist in swapping p and d

    // indexes into strings s and t
    var i:Int = 0 // iterates through s
    var j:Int = 0 // iterates through t

    var t_j:Char = 0 // jth character of t

    var cost:Int = 0 // cost

    i = 0
    while(i <= n){
      p(i) = i;
      i += 1
    }
    
    j = 1
    while(j <= m){
      t_j = t.charAt(j-1);
      d(0) = j;
      
      i = 1
      while(i <= n){
        cost = if(s.charAt(i-1) == t_j) 0 else 1
        // minimum of cell to the left+1, to the top+1, diagonally left and up +cost				
        d(i) = scala.math.min(scala.math.min(d(i-1)+1, p(i)+1),  p(i-1)+cost);  
	i += 1
      }

      // copy current distance counts to 'previous row' distance counts
      _d = p
      p = d
      d = _d
      j += 1
    }
    
    // our last action in the above loop was to switch d and p, so p now 
    // actually has the most recent cost counts
    p(n)
  }
}
