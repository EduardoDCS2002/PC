import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

  class Connector {

  private Socket sock;
  private BufferedReader in;
  private PrintWriter pw;
  
  public Connector(){
  }

  public boolean connect(String addr, int port) {
        try{
             sock = new Socket(addr,port);
            in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
            pw = new PrintWriter(sock.getOutputStream());
            sock.setTcpNoDelay(true);
            return true;
        } catch(Exception e){return false;}
    }
  
  public Socket getSocket(){return sock;} 
  
    public void write(String s){
        System.out.println("Write: " + s);
        pw.println(s);
        pw.flush(); 
    }
    
        
    public String read(){
      String str; //<>// //<>//
        try{ //<>// //<>//
          str = in.readLine();  
          System.out.println("Read : " + str);
        } catch(Exception e)
        {
          return "";
        }
        return str;
    }

    public void disconnect(){
        try{
            sock.close();
        } catch(Exception e){}
    }

}
