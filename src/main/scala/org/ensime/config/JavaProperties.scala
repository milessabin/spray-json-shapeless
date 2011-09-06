/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.config
import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions._
import java.util.Properties


object JavaProperties {

  def load(file:File):Map[Any,Any] = {
    val props = new Properties()
    try{
      val fis = new FileInputStream(file)
      try{
	props.load(fis);    
	props.toMap
      }
      finally{
	fis.close();
      }
    }
    catch{
      case e => Map()
    }
  }

}
