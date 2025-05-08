class Shooter {
  float x,y;
  float angle;
  int col;
  String name;
  

  // Construtor
  Shooter(String name, float x, float y,int col, float angle) {
    this.name = name;
    this.x = x;
    this.y = y;
    this.col = col;
    this.angle = angle;
  }

  // Método para desenhar o astronauta
  void display(PApplet appc) {
    appc.fill(col,0,0);
    appc.pushMatrix();
    appc.translate(this.x, this.y);
    appc.ellipse(0, 0, 20, 20);
    appc.rotate(radians(this.angle));
    appc.rect(20, 0, 20, 10);
    appc.popMatrix();
  }
}
