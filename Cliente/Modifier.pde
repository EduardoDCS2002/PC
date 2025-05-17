class Modifier {
  float x;
  float y;
  float radius;
  int r,g,b;

  // Construtor
  Modifier(float x, float y, float raio, int R, int G, int B) {
    this.x = x;
    this.y = y;
    this.radius = raio;
    this.r = R;
    this.g = G;
    this.b = B;
  }

  // Método para desenhar o planeta
  void display(PApplet appc) {
    appc.fill(r,g,b);
    appc.ellipse(x, y, radius, radius);
  }
}