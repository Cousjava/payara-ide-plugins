/*
 * ProcessExecutor.java
 * 
 * Created on Oct 4, 2007, 9:06:17 AM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.execution;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;
/**
 *
 * @author ludo
 */
public class ProcessExecutor {
    static ExecutorService pool= Executors.newFixedThreadPool(1);//1 process at a time
        public static void main() {

    for (int i=0;i<10;i++){
        final int j=i;
       Future f =  pool.submit(new Runnable(){

                public void run() {
                    try {
                        Thread.sleep(1000 / j);
                        System.out.println("i" + j);
                    } catch (InterruptedException ex) {
                        Logger.getLogger(ProcessExecutor.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            
        });
      if (i==3)  f.cancel(true);
    }
 
    }

}
