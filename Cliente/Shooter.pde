class Shooter {
  float x,y;
  int col;
  String name;
  
  // Construtor
  Shooter(String name, float x, float y,int col) {
    this.name = name;
    this.x = x;
    this.y = y;
    this.col = col;
    
  }

  // Método para desenhar o astronauta
  void display(PApplet appc) {
    
    appc.fill(col,0,0);
    appc.ellipse(x, y, 20, 20);
  }
}
