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

/***
* ASM: a very small and fast Java bytecode manipulation framework
* Copyright (c) 2000-2011 INRIA, France Telecom
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
* 3. Neither the name of the copyright holders nor the names of its
*    contributors may be used to endorse or promote products derived from
*    this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
* THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.indexer
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.commons.EmptyVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes
import scala.collection.mutable.HashMap


trait MethodDescriber extends MethodVisitor {

  import org.objectweb.asm.util.AbstractVisitor._

  val INTERNAL_NAME = 0;
  val FIELD_DESCRIPTOR = 1;
  val FIELD_SIGNATURE = 2;
  val METHOD_DESCRIPTOR = 3;
  val METHOD_SIGNATURE = 4;
  val CLASS_SIGNATURE = 5;
  val TYPE_DECLARATION = 6;
  val CLASS_DECLARATION = 7;
  val PARAMETERS_DECLARATION = 8;
  val HANDLE_DESCRIPTOR = 9;

  def appendOp(name:String, args:String):Unit
  def appendOp(name:String):Unit = appendOp(name, "")

  private def descriptor(tpe: Int, desc: String): String = {
    if (tpe == CLASS_SIGNATURE || tpe == FIELD_SIGNATURE
      || tpe == METHOD_SIGNATURE) {
      if (desc != null) {
        "// signature " + desc
      } else ""
    } else { desc }
  }

  val labelNames = new HashMap[Label, String]()
  private def label(l: Label): String = {
    labelNames.get(l) match {
      case Some(name) => name
      case _ => {
	val name = "L" + labelNames.size
	labelNames(l) = name
	name
      }
    }
   }

  override def visitInsn(opcode: Int) {
    appendOp(OPCODES(opcode))
  }

  override def visitIntInsn(opcode: Int, operand: Int) {
    appendOp(OPCODES(opcode),
      if (opcode == Opcodes.NEWARRAY)
      { TYPES(operand) } else { operand.toString })
  }

  override def visitVarInsn(opcode: Int, variable: Int) {
    appendOp(OPCODES(opcode), variable.toString)
  }

  override def visitTypeInsn(opcode: Int, tpe: String) {
    appendOp(OPCODES(opcode), descriptor(INTERNAL_NAME, tpe))
  }

  override def visitFieldInsn(opcode: Int, owner: String,
    name: String, desc: String) {
    appendOp(OPCODES(opcode),
      descriptor(INTERNAL_NAME, owner) +
      "." + name +
      " : " + descriptor(FIELD_DESCRIPTOR, desc))
  }

  override def visitMethodInsn(opcode: Int, owner :String,
    name: String, desc:String) {
    appendOp(OPCODES(opcode),
      descriptor(INTERNAL_NAME, owner) +
      "." + name + descriptor(METHOD_DESCRIPTOR, desc))
  }


  // TODO pending method handle in ASM
  // override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle,
  //   val bsmArgs:Object*) {
  //   buf.setLength(0)
  //   buf.append(tab2).append("INVOKEDYNAMIC").append(' ')
  //   buf.append(name)
  //   descriptor(METHOD_DESCRIPTOR, desc)
  //   buf.append(" [")
  //   appendHandle(bsm)
  //   buf.append(tab3).append("// arguments:")
  //   if (bsmArgs.length == 0) {
  //     buf.append(" none")
  //   } else {
  //     buf.append('\n').append(tab3)
  //     for (int i = 0 i < bsmArgs.length i++) {
  //       Object cst = bsmArgs[i]
  //       if (cst instanceof String) {
  //         Printer.appendString(buf, (String) cst)
  //       } else if (cst instanceof Type) {
  //         buf.append(((Type) cst).getDescriptor()).append(".class")
  //       } else if (cst instanceof Handle) {
  //         appendHandle((Handle) cst)
  //       } else {
  //         buf.append(cst)
  //       }
  //       buf.append(", ")
  //     }
  //     buf.setLength(buf.length() - 2)
  //   }
  //   buf.append('\n')
  //   buf.append(tab2).append("]\n")
  //   text.add(buf.toString())
  // }

  override def visitJumpInsn(opcode: Int, lbl: Label) {
    appendOp(OPCODES(opcode), label(lbl))
  }

  override def visitLabel(lbl: Label) {
    appendOp(label(lbl))
  }

  override def visitLdcInsn(cst: Object) {
    appendOp("LDC",
      if (cst.isInstanceOf[String]) {
	"\"" + cst.toString + "\""
      } else if (cst.isInstanceOf[Type]) {
	cst.asInstanceOf[Type].getDescriptor()
      } else {
	cst.toString
      })
  }

  override def visitIincInsn(variable: Int, increment: Int) {
    appendOp("IINC ", variable.toString + ' ' + increment.toString)
  }

  override def visitTableSwitchInsn(min: Int, max: Int, dflt: Label, labels:Array[Label]) {
    appendOp("TABLESWITCH",
      labels.zipWithIndex.map { pair => (min + pair._2) + ": " + label(pair._1) }.mkString(",") +
	"default: " + label(dflt))
  }

  override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int],
    labels: Array[Label]) {
    appendOp("LOOKUPSWITCH",
      labels.zipWithIndex.map { pair => (keys(pair._2)) + ": " + label(pair._1) }.mkString(",") +
	"default: " + label(dflt))
  }

  override def visitMultiANewArrayInsn(desc:String, dims:Int) {
    appendOp("MULTIANEWARRAY", descriptor(FIELD_DESCRIPTOR, desc) +
      " " + dims)
  }

}
