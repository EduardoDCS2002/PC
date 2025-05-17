import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.ArrayList;
import java.util.HashMap;
import processing.core.PApplet;

class Game {

  ArrayList<Shooter> players;
  ArrayList<Modifier> modifiers;
  ArrayList<Bullet> bullets;
  ArrayList<String> playerNames;
  ArrayList<Integer> playerColors;
  Lock l;

  public Game (ArrayList<Shooter> players, ArrayList<Modifier> modifiers, ArrayList<Bullet> bullets){
      this.players  = players;
      this.modifiers = modifiers;
     
      this.bullets= bullets;
      this.l = new ReentrantLock();
  }

  void update (ArrayList<Shooter> players, ArrayList<Modifier> modifiers,ArrayList<Bullet> bullets) {

    this.l.lock();
    println("Teste UPDATE");
    try {
      this.players  = players;
      this.modifiers = modifiers;
      this.bullets = bullets;
    }finally{
      this.l.unlock();
    }
  }


   void draw(PApplet appc) {
     for (Shooter s : this.players) {
        s.display(appc);
      }
     
    for (Modifier m : this.modifiers) {
        m.display(appc);
    }
    
    for (Bullet b : this.bullets){
      b.display(appc);
    }
    
  }

  
}