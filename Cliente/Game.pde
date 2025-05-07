import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.ArrayList;
import java.util.HashMap;
import processing.core.PApplet;

class Game {

  ArrayList<Shooter> players;
  ArrayList<Modifier> modifiers;
  ArrayList<String> playerNames;
  ArrayList<Integer> playerColors;
  Lock l;



  public Game (ArrayList<Shooter> players, ArrayList<Modifier> modifiers) {

      this.players  = players;
      this.modifiers = modifiers;
      this.l = new ReentrantLock();
  }

  void update (ArrayList<Shooter> players, ArrayList<Modifier> modifiers) {

    this.l.lock();
    //println("Teste UPDATE");
    try {
      this.players  = players;
      this.modifiers = modifiers;
    }finally{
      this.l.unlock();
    }
  }


   void draw(PApplet appc) {
     
    for (Modifier m : this.modifiers) {
        m.display(appc);
    }
    int margin = 35; // Margem a partir da borda
    int lineHeight = 35; // Altura da linha
    int y = margin; // Iniciar a partir da margem superior

    for (Shooter s : this.players) {
        appc.fill(255,255,255);
        appc.textSize(16);
        appc.text(s.name, (s.x) - 10, (s.y)-15);
        
        //appc.fill(255,255,255);
        //appc.textSize(30);
        //y += lineHeight; // Incrementar a posição vertical para a próxima linha
        s.display(appc);
      }
  }

  
}
