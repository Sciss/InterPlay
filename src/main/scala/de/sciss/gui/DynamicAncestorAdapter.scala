/*
 *  DynamicAncestorAdapter.java
 *  (de.sciss.gui package)
 *
 *  Copyright (c) 2004-2011 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 *		20-May-05	created from de.sciss.meloncillo.gui.DynamicAncestorAdapter
 *		21-Sep-06	added remove(), isListening(), getComponent()
 */

package de.sciss.gui

import javax.swing.event.{AncestorListener, AncestorEvent}
import java.awt.{EventQueue, Container, Window}
import java.awt.event.{ComponentEvent, ComponentAdapter, WindowEvent, WindowAdapter}
import javax.swing.JComponent

object DynamicListening {
   def apply( startThunk: => Unit )( stopThunk: => Unit ) = new DynamicListening {
      def startListening   = startThunk
      def stopListening    = stopThunk
   }
}
trait DynamicListening {
   def startListening : Unit
   def stopListening : Unit
}

object DynamicAncestorAdapter {
   def apply( view: JComponent )( startThunk: => Unit )( stopThunk: => Unit ) = {
      val res = new DynamicAncestorAdapter( DynamicListening( startThunk )( stopThunk )).addTo( view )
      res
   }
}

/**
 *  This class can be added as an <code>AncestorListener</code>
 *  and will call the passed <code>DynamicListening</code> object
 *  when the Component becomes visible or invisible in the
 *  sense that it's ancestor window is shown or hidden.
 *  <p>
 *  <strong>It's crucial that the <code>addTo</code> method is
 *  used to register the listener!</strong>
 *  <p>
 *  <code>Surface</code> is an example of the use of a
 *  <code>DynamicAncestorAdapter</code>.
 *
 *  @param  listener	a <code>DynamicListening</code>
 *						whose <code>startListening</code>
 *						method is called when this adapter's
 *						host component's ancestor is shown
 *						or added to another component. the
 *						listener's <code>stopListening</code>
 *						method is called likewise when
 *						this adapter's host component's ancestor
 *						is hidden or removed from its parent.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.13, 24-Jan-10
 */
class DynamicAncestorAdapter( listener: DynamicListening ) extends AncestorListener with DynamicListening {
   adapter =>

   private val winL = new WindowAdapter() {
      override def windowOpened( e: WindowEvent  ) {
         if( !listening ) startListening
      }

      override def windowClosed( e: WindowEvent ) {
         if( listening ) stopListening
      }
   }
   private val cmpL = new ComponentAdapter() {
      override def componentShown( e: ComponentEvent ) {
         if( !listening ) startListening
      }

      override def componentHidden( e: ComponentEvent ) {
         if( listening ) stopListening
      }
   }

	private var listening                  = false
   private var cmp : Option[ JComponent ] = None
   private var win : Option[ Window ]     = None


	/**
	 *  Adds this adapter to a <code>JComponent</code>.
	 *  <strong>Use this method instead of calling
	 *  <code>cmp.addAncestorListener(...)</code></strong>
	 *  because this method will automatically detect
	 *  the component's window. This is crucial for
	 *  <code>JRootPane</code> components, because they are already
	 *  attached to a window when you register the
	 *  listener.
	 *
	 *  @param  c		the <code>JComponent</code> who will be tracked for
	 *					ancestor changes.
	 *  @see	javax.swing.JComponent#addAncestorListener( AncestorListener )
	 */
	def addTo( c: JComponent ) {
      require( EventQueue.isDispatchThread )

		if( cmp.isDefined ) error( "Already added" )
		cmp = Some( c )
		c.addAncestorListener( adapter )
		learnWindow( c.getTopLevelAncestor() )
	}

	def remove() {
      require( EventQueue.isDispatchThread )

      cmp.foreach { c =>
         c.removeAncestorListener( adapter )
         forgetWindow()
         cmp = None
      }
	}

   def component : Option[ JComponent ] = cmp
   def isListening : Boolean = listening

	/**
	 *  Called when the tracked component or one of
	 *  its ancestors gets added in the container hierarchy.
	 *  This method checks to see if a change in the
	 *  component's top level window occured, and if
	 *  so, re-registers window and component listeners
	 *  on that window. Also if the window is visible
	 *  and the dynamic listener is not yet listening,
	 *  its <code>startListening</code> method is invoked.
	 */
	def ancestorAdded( e: AncestorEvent ) {
		val c = e.getComponent().getTopLevelAncestor()
		if( !(c eq win) ) {
			forgetWindow()
			learnWindow( c )
		}
	}

	/**
	 *  Called when the tracked component or one of
	 *  its ancestors gets removed in the container hierarchy.
	 *  If the dynamic listener was started,
	 *  its <code>stopListening</code> method is invoked.
	 */
	def ancestorRemoved( e: AncestorEvent ) {
		forgetWindow()
	}

   def ancestorMoved( e: AncestorEvent ) {}

	private def forgetWindow() {
      win.foreach { w =>
			w.removeWindowListener( winL )
			w.removeComponentListener( cmpL )
			win = None
			if( listening ) stopListening
		}
	}

	private def learnWindow( c: Container ) {
      Option( c ) match {
         case Some( w: Window ) =>
            w.addWindowListener( winL )
            w.addComponentListener( cmpL )
            if( !listening && w.isShowing ) startListening

         case _ =>
      }
	}

	def startListening {
		listener.startListening
		listening = true
	}

	def stopListening {
		listener.stopListening
		listening = false
	}
}