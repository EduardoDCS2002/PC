class Bullet {
  float x;
  float y;
  

  // Construtor
  Bullet(float x, float y) {
    this.x = x;
    this.y = y;
  }

  // Método para desenhar a bala
  void display(PApplet appc) {
    
    appc.fill(0,0,0);
    appc.ellipse(x, y, 3, 3);
  }
}
