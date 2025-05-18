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

  // Método para desenhar o shooter
  void display(PApplet appc) {
    appc.fill(col,0,0);
    //appc.pushMatrix();
    //appc.translate(this.x, this.y);
    appc.ellipse(0, 0, 100, 100);
    //appc.rotate(radians(this.angle));
    //rever se é necessario
    //appc.rect(20, 0, 20, 10);
    //appc.popMatrix();
    
    appc.fill(col,0,col);
    appc.ellipse(x, y, 50 , 50);
  }
}
