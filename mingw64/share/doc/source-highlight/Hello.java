/*
  This is a classical Hello program
  to test source-highlight with Java programs.
  
  to have an html translation type

	source-highlight -s java -f html --input Hello.java --output Hello.html
	source-highlight -s java -f html < Hello.java > Hello.html

  or type source-highlight --help for the list of options

  written by
  Lorenzo Bettini
  http://www.lorenzobettini.it
  http://www.gnu.org/software/src-highlite
*/

package hello;

import java.io.* ;

/**
 * <p>
 * A simple Hello World class, used to demonstrate some
 * features of Java source highlighting.
 * </p>
 * TODO: nothing, just to show an highlighted TODO or FIXME
 *
 * @author Lorenzo Bettini
 * @version 2.0
 */
public class Hello {
    int foo = 1998 ;
    int hex_foo = 0xCAFEBABE;
    boolean b = false;
    Integer i = null ;
    char c = '\'', d = 'n', e = '\\' ;
    String xml = "<tag attr=\"value\">&auml;</tag>", foo2 = "\\" ;
    
    public static void main( String args[] ) {
	// just some greetings ;-)  /*
	System.out.println( "Hello from java2html :-)" ) ;
	System.out.println( "\tby Lorenzo Bettini" ) ;
	System.out.println( "\thttp://www.lorenzobettini.it" ) ;
        if (argc > 0)
            String param = argc[0];
        //System.out.println( "bye bye... :-D" ) ; // see you soon
    }
}
