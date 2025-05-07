class Modifier {
  float x;
  float y;
  int type;

  // Construtor
  Modifier(float x, float y, int type) {
    this.x = x;
    this.y = y;
    // 0-> verde, 1 -> laranka, 2-> azul , 3-> vermelho
    this.type = type;
  }

  // Método para desenhar o planeta
  void display(PApplet appc) {
    
    if(type == 0){
      appc.fill(0,200,0);
    }
    else if (type == 1){
      appc.fill(204,102,0);
    }
    else if (type == 2){
      appc.fill(0,0,200);
    }
    else {
      appc.fill(200,0,0);
    }
    appc.ellipse(x, y, 3, 3);
  }
}
