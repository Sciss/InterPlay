package de.sciss.interplay

import java.awt.EventQueue
import javax.swing.{WindowConstants, JFrame}

object InterPlay {
   def main( args: Array[ String ]) {
      EventQueue.invokeLater( new Runnable {
         def run = init
      })
   }

   def init {
      val f = new JFrame( "InterPlay" )
      f.setResizable( false )
      f.setSize( 200, 200 )
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }
}