class Bullet {
  float x;
  float y;
  float raio;
  

  // Construtor
  Bullet(float x, float y, float r) {
    this.x = x;
    this.y = y;
    this.raio = r;
  }

  // Método para desenhar a bala
  void display(PApplet appc) {
    
    appc.fill(255,255,255);
    appc.ellipse(x, y, raio, raio);
  }
}