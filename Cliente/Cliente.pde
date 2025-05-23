import g4p_controls.*;
import java.util.*;
import java.lang.*;
import java.util.StringTokenizer;
import java.awt.Font;
import java.io.FileReader;
import java.awt.event.MouseEvent;

GTextArea server_connection_label; // log
GLabel titulo_label; // titulo

// botoes no momemto do login_manager
GButton registar_button; 
GButton cancelar_button; 
GButton login_button;
GButton jogo_pontos_button;

// janelas
GWindow menu_window; // janela inicial
GWindow registo_window; // janela do registro
GWindow login_window; // janela login
GWindow jogo_window; // janela do jogo
GWindow pontos_Window; // janela ranking

GLabel scoresA; // texto dos ranking
GLabel perdeu_label; // texto quando perde

GLabel password_label; // indica espaço de texto para escrever senha
GTextField nome; // espaço para escrever nome
GLabel nome_label; // indica espaço para escrever username
GPassword password; // espaço para escrever senha

GButton concluir_registo_button; // botao finaliza registo
GButton concluir_login_button;  // botao finaliza login

GTextField ip; // normamente localhost
GTextField porta; // normalmente 22346

// strings para leitura
String lastNome = "";
String lastPassword = "";
String ipLido = "";
String portaLida = "";

//flags
boolean threadMorreu = false;
boolean estadoJogo = false;
boolean apresentarPontos = false;

// varaiveis de interacao
int i = 0;
int f = 0;
int conta = 0;

Game jogo = new Game(new ArrayList<Shooter>(), new ArrayList<Modifier>(), new ArrayList<Bullet>());
Connector con = new Connector();

HashMap<String, Integer> MelhoresPontuacoes = new HashMap<String, Integer>();

Thread thread;
Thread ranking;
int andaThread = 0;

Font font = new Font("Arial", Font.PLAIN, 18);

public void setup() {
  size(1300, 700, JAVA2D);
  frameRate(60);
  menu();
}

public void close_jogo (GWindow window){
  estadoJogo = false;
  con.write("quit");
  getSurface().setVisible(true);
  perdeu_label.setText("");
  jogo_window.setVisible(false);
}

public void fecha_ranking_window(GWindow window) {
  pontos_Window.setVisible(false);
  apresentarPontos = false;
}

void keyPressed_Handler(PApplet appc, GWinData data, KeyEvent event) {

 if (appc.keyPressed) {
   String command = "";
   if(appc.keyCode == LEFT || appc.key == 'a' || appc.key == 'A'){
     command = "L";
   }
   if(appc.keyCode == UP || appc.key == 'w' || appc.key == 'W'){
     command = "U";
   }
   if(appc.keyCode == RIGHT || appc.key == 'd' || appc.key == 'D'){
     command = "R";
   }
   if(appc.keyCode == DOWN || appc.key == 's' || appc.key == 'S'){
     command = "D";
   }
   if(appc.keyCode == ' ' ){
     command = "bang" + " " + appc.mouseX + " " + appc.mouseY;
   }
   
   if(command != ""){
     println(command);
     con.write(command);
   }
  }
}

public void mousePressed_Handler(PApplet appc, GWinData data, MouseEvent event){
  if (event.getButton() == MouseEvent.BUTTON1) { // botão esquerdo
        int x = event.getX();
        int y = event.getY();
        String command = "SHOOT " + x + " " + y;
        println(command);
        con.write(command);
    }
   
}

public void draw() {

  if (estadoJogo == false) {
    background(50);
    fill(0);
  }
}

public void menu() {
   ipLido = "localhost";
   portaLida = "22345"; 
   boolean ok = con.connect(ipLido, Integer.parseInt(portaLida));  

  this.noLoop();
  G4P.messagesEnabled(false);
  G4P.setGlobalColorScheme(7);
  G4P.setMouseOverEnabled(false);
  G4P.setDisplayFont("Arial", G4P.PLAIN, 16);
  G4P.setInputFont("Arial", G4P.PLAIN, 20);

  surface.setTitle("Jogo");

  server_connection_label = new GTextArea(this, 200, 150, 900, 110, G4P.SCROLLBARS_VERTICAL_ONLY | G4P.SCROLLBARS_AUTOHIDE);
  server_connection_label.setOpaque(true);
  server_connection_label.addEventHandler(this, "server_connection_label_change");

  titulo_label = new GLabel(this, 300, 10, 700, 60);
  titulo_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  titulo_label.setText("SHOOTING GAME");
  titulo_label.setOpaque(false);

  titulo_label = new GLabel(this, 300, 40, 700, 60);
  titulo_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  titulo_label.setText("ShootingTime");
  titulo_label.setOpaque(false);
  
  titulo_label = new GLabel(this, 300, 60, 700, 120);
  titulo_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  titulo_label.setText("Bem-vindo ao Jogo ShootingTime! Tens a opção de registo, login e de cancelar um registo. Quando realizares o login, irás ser colocado no lobby então espere alguns segundos até a partida começar");
  titulo_label.setOpaque(false);
  
  int buttonWidth = 300;
  int buttonHeight = 110;
  int centerX = width/2 - buttonWidth/2;

  login_button = new GButton(this, centerX, 270, buttonWidth, buttonHeight); // Posição do botão Login alterada
  login_button.setText("Login");
  login_button.setLocalColorScheme(7);
  login_button.addEventHandler(this, "login_button_click");

  registar_button = new GButton(this, centerX, 420, buttonWidth, buttonHeight); // Posição do botão Registrar alterada
  registar_button.setText("Registar");
  registar_button.setLocalColorScheme(5);
  registar_button.addEventHandler(this, "registar_button_click");
  
  cancelar_button = new GButton(this, centerX, 570, buttonWidth, buttonHeight);
  cancelar_button.setText("Cancelar");
  cancelar_button.setLocalColorScheme(4);
  cancelar_button.addEventHandler(this, "cancelar_button_click");

  jogo_pontos_button = new GButton(this,width-250, 20, buttonWidth - 60, buttonHeight); 
  jogo_pontos_button.setText("Rankings"); // Define o texto do botão
  jogo_pontos_button.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  jogo_pontos_button.setLocalColorScheme(6);
  jogo_pontos_button.addEventHandler(this, "jogo_pontos_button_click");

  this.loop();
}

//#region menu
synchronized public void registo_window_draw(PApplet appc, GWinData data) { //_CODE_:registo_window:453092:
  appc.background(50);
  appc.fill(0);
} 

public void server_connection_label_change(GTextArea source, GEvent event) { //_CODE_:server_connection_label:697017:
  println("server_connection_label - GTextArea >> GEvent." + event + " @ " + millis());
} 
 

public void registar_button_click(GButton source, GEvent event) { //_CODE_:registar_button:703554:
  println("registar_button - GButton >> GEvent." + event + " @ " + millis());
  registar();
  getSurface().setVisible(false);
} 


public void cancelar_button_click(GButton source, GEvent event) { //_CODE_:cancelar_button:610057:
  println("cancelar_button - GButton >> GEvent." + event + " @ " + millis());
  cancelar();
  getSurface().setVisible(false);
} 

public void login_button_click(GButton source, GEvent event) { //_CODE_:login_button:963696:
  println("login_button - GButton >> GEvent." + event + " @ " + millis());
  login();
  getSurface().setVisible(false);
} 
//#endregion


public void registar() {

  registo_window = GWindow.getWindow(this, "Jogo Registo", 600, 100, 500, 500, JAVA2D);
  registo_window.noLoop();
  registo_window.setActionOnClose(G4P.CLOSE_WINDOW);
  registo_window.addDrawHandler(this, "registo_window_draw");

  password_label = new GLabel(registo_window, 20, 260, 160, 40);
  password_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  password_label.setText("Palavra-Passe");
  password_label.setOpaque(false);

  nome_label = new GLabel(registo_window, 20, 120, 160, 40);
  nome_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  nome_label.setText("Nome :");
  nome_label.setOpaque(false);

  nome = new GTextField(registo_window, 220, 120, 220, 40, G4P.SCROLLBARS_NONE);
  nome.setOpaque(true);
  nome.addEventHandler(this, "nome_change");

  concluir_registo_button = new GButton(registo_window, 140, 380, 200, 60);
  concluir_registo_button.setText("REGISTAR");
  concluir_registo_button.addEventHandler(this, "concluir_registo_button_click");

  password = new GPassword(registo_window, 220, 260, 220, 40);
  password.setMaxWordLength(20);
  password.setOpaque(true);
  password.addEventHandler(this, "password_change");

  registo_window.addOnCloseHandler(this, "close_registo_window");

  registo_window.loop();
}

public void nome_change(GTextField source, GEvent event) { 
  println("Nome :" + nome.getText());
  lastNome = nome.getText();
} 

public void concluir_registo_button_click(GButton source, GEvent event) { 

  println("concluir_registo_button - GButton >> GEvent." + event + " @ " + millis());
  getSurface().setVisible(true);
  registo_window.setVisible(false);
  con.write("create_account " + lastNome + " " + lastPassword);
  lastNome = "";
  lastPassword = "";
  String res = "";
    try {
       while (res.equals("")){
         Thread.sleep(300);
         res = con.read();
         println("Resposta do server: " + res);
         server_connection_label.appendText(res);
       }
    }
    catch(Exception e) {
    }
      
  println("Resposta do server: " + res);
  server_connection_label.appendText(res);
} 


public void password_change(GPassword source, GEvent event) { 
  println("Password : " + password.getPassword());
  lastPassword = password.getPassword();
} 

public void close_registo_window (GWindow window) { 
  getSurface().setVisible(true);
  registo_window.setVisible(false);
} 

public void login() {
  
  registo_window = GWindow.getWindow(this, "Jogo Login", 600, 100, 500, 500, JAVA2D);
  registo_window.noLoop();
  registo_window.setActionOnClose(G4P.CLOSE_WINDOW);
  registo_window.addDrawHandler(this, "registo_window_draw");

  password_label = new GLabel(registo_window, 20, 260, 160, 40);
  password_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  password_label.setText("Palavra-Passe");
  password_label.setOpaque(false);

  nome_label = new GLabel(registo_window, 20, 120, 160, 40);
  nome_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  nome_label.setText("Nome :");
  nome_label.setOpaque(false);

  nome = new GTextField(registo_window, 220, 120, 220, 40, G4P.SCROLLBARS_NONE);
  nome.setOpaque(true);
  nome.addEventHandler(this, "nome_change");

  concluir_registo_button = new GButton(registo_window, 140, 380, 200, 60);
  concluir_registo_button.setText("LOGIN");
  concluir_registo_button.addEventHandler(this, "concluir_login_button_click");

  password = new GPassword(registo_window, 220, 260, 220, 40);
  password.setMaxWordLength(20);
  password.setOpaque(true);
  password.addEventHandler(this, "password_change");
  
  registo_window.addOnCloseHandler(this, "close_login_window");

  registo_window.loop();
}

public void close_login_window (GWindow window) { 
  getSurface().setVisible(true);
  registo_window.setVisible(false);
} 

public void criaJogoWindow() {
  jogo_window = null;
  jogo_window = GWindow.getWindow(this, "Jogo", 0, 0, 1300, 700, JAVA2D);
  jogo_window.addKeyHandler(this, "keyPressed_Handler");
  //jogo_window.addMouseHandler(this, "mousePressed_Handler");
  

  jogo_window.setActionOnClose(G4P.CLOSE_WINDOW);
  jogo_window.setVisible(false);
  jogo_window.addDrawHandler(this, "drawJogo");
  jogo_window.addOnCloseHandler(this, "close_jogo");
  jogo_window.setVisible(false);
  
}

public void concluir_login_button_click(GButton source, GEvent event) { 

  getSurface().setVisible(true);
  registo_window.setVisible(false);
  con.write("login " + lastNome + " " + lastPassword);
  String res = "";
 
    try {
       while (res.equals("")){
         Thread.sleep(300);
         res = con.read();
         println("Resposta do server: " + res);
         server_connection_label.appendText(res);
       }
    }
    catch(Exception e) {
      
    }
        
  Runnable r = new Runnable() {
    public void run() {
      try {        
        while (!con.read().equals("Comeca")) {
          Thread.sleep(300);
        }
        criaJogoWindow();
        println("Vou começar o jogo\n");
        getSurface().setVisible(false);
        estadoJogo = true;
        jogo_window.frameRate(60);
        jogo_window.setVisible(true);
        while (estadoJogo) {
          //println("a\n");
          String estadoLido = con.read();

          println(estadoLido);

          if (estadoLido.equals("Perdeu") ) {
            perdeu_label = new GLabel(jogo_window, 0, 100, 1300, 400);
            perdeu_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
            perdeu_label.setFont(new Font("Arial", Font.PLAIN, 40));
            perdeu_label.setText("");
            perdeu_label.setOpaque(false);
            perdeu_label.setText("PERDEU");
            jogo_pontos_button.setVisible(true);
            estadoJogo = false;
            
          } 
          else if (estadoLido.equals("Venceu") ) {
            perdeu_label = new GLabel(jogo_window, 0, 100, 1300, 400);
            perdeu_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
            perdeu_label.setFont(new Font("Arial", Font.PLAIN, 40));
            perdeu_label.setText("");
            perdeu_label.setOpaque(false);
            perdeu_label.setText("VENCEU");
            jogo_pontos_button.setVisible(true);
            println("Visivel");
            estadoJogo = false;
            
          }       
          else {
            if (!estadoLido.equals(""))
            {
              updateJogo(estadoLido);
            }
            
          }
         
        }
      }
      catch(Exception e ) {
      }
    }
  };

  thread = new Thread(r);
  thread.start();
} 

synchronized public void drawJogo(PApplet appc, GWinData data) { 

  try 
   {
      appc.background(0);
      appc.imageMode(CORNER);
      appc.fill(0);
      estadoJogo = true;
      if (estadoJogo) {
        
        jogo.draw(appc);
     }
   }
   catch (Exception e){
     
   }
  }

public void drawRanking(PApplet appc, GWinData data) {
    if (apresentarPontos) {
        List<Player> players = loadPlayersInfo();
        players.sort(Comparator.comparingInt(Player::getLevel)
                .thenComparingInt(p -> p.getVictories() - p.getDefeats())
                .reversed()); // Ordena os jogadores por nível e depois pela última série de vitórias/derrotas

        StringBuilder sb = new StringBuilder();
        int count = 0;
        for (Player player : players) {
            if (count >= 10) break; // Limita a exibição aos 10 melhores jogadores
            sb.append(player.getUsername())
              .append(" : Nível ")
              .append(player.getLevel())
              .append("\n");
            count++;
        }

        // Limpa a janela antes de desenhar
        appc.background(255);
        appc.fill(0);
        appc.textSize(17); // Aumente este valor conforme necessário
        appc.text(sb.toString(), 150, 50); // Desenhe o texto na posição (150, 50)
    }
}

private List<Player> loadPlayersInfo() {
    List<Player> players = new ArrayList<>();
    con.write("rankings");
    String infostring = "";
    
    // Read data with timeout (5 seconds max)
    long startTime = System.currentTimeMillis();
    try {
        while (infostring.isEmpty() && (System.currentTimeMillis() - startTime < 5000)) {
            Thread.sleep(300);
            infostring = con.read();
            if (!infostring.isEmpty()) {
                System.out.println("Rankings info: " + infostring);
                server_connection_label.appendText(infostring);
            }
        }
    } catch (Exception e) {
        throw new RuntimeException("Failed to read rankings", e);
    }

    // Parse using StringTokenizer
    try {
        // Remove unwanted characters first
        infostring = infostring.replaceAll("[{}\"]", "").trim();
        
        // Use StringTokenizer to split by commas
        StringTokenizer tokenizer = new StringTokenizer(infostring, ",");
        List<String> tokens = new ArrayList<>();
        while (tokenizer.hasMoreTokens()) {
            tokens.add(tokenizer.nextToken().trim());
        }

        // Ensure we have enough tokens
        if (tokens.size() == 6) {
            try {
                //String username = tokens.get(0);
                //int victories = Integer.parseInt(tokens.get(3));
                //int defeats = Integer.parseInt(tokens.get(4));
                //int level = Integer.parseInt(tokens.get(5));
                System.out.println("Some error occurs when trying to parse the rankings...")
                //players.add(new Player(username, victories, defeats, level));
            } catch (NumberFormatException e) {
                System.err.println("Skipping malformed line: " + infostring);
            }
        } else {
            System.err.println("Not enough data fields in: " + infostring);
        }
    } catch (Exception e) {
        throw new RuntimeException("Failed to parse rankings", e);
    }
    
    return players;
}

public void jogo_pontos_button_click(GButton source, GEvent event) {
    pontos_Window = GWindow.getWindow(this, "Rankings", 1150, 100, 400, 400, JAVA2D);
    pontos_Window.setActionOnClose(G4P.CLOSE_WINDOW);
    pontos_Window.addDrawHandler(this, "drawRanking");
    pontos_Window.addOnCloseHandler(this, "fecha_ranking_window");
    scoresA = new GLabel(pontos_Window, 35, 0, 200, 100, "");
    scoresA.setTextAlign(GAlign.CENTER, GAlign.TOP);
    scoresA.setLocalColorScheme(GCScheme.RED_SCHEME);
    scoresA.setOpaque(false);
    scoresA.setFont(font);

    apresentarPontos = true;
    threadMorreu = true;
}


public void cancelar() {

  registo_window = GWindow.getWindow(this, "Jogo Cancelar", 600, 100, 500, 500, JAVA2D);
  registo_window.noLoop();
  registo_window.setActionOnClose(G4P.CLOSE_WINDOW);
  registo_window.addDrawHandler(this, "registo_window_draw");

  password_label = new GLabel(registo_window, 20, 260, 160, 40);
  password_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  password_label.setText("Palavra-Passe");
  password_label.setOpaque(false);

  nome_label = new GLabel(registo_window, 20, 120, 160, 40);
  nome_label.setTextAlign(GAlign.CENTER, GAlign.MIDDLE);
  nome_label.setText("Nome :");
  nome_label.setOpaque(false);

  nome = new GTextField(registo_window, 220, 120, 220, 40, G4P.SCROLLBARS_NONE);
  nome.setOpaque(true);
  nome.addEventHandler(this, "nome_change");

  concluir_registo_button = new GButton(registo_window, 140, 380, 200, 60);
  concluir_registo_button.setText("ELIMINAR");
  concluir_registo_button.addEventHandler(this, "concluir_cancelar_button_click");

  password = new GPassword(registo_window, 220, 260, 220, 40);
  password.setMaxWordLength(20);
  password.setOpaque(true);
  password.addEventHandler(this, "password_change");

  registo_window.addOnCloseHandler(this, "close_cancelar_window");

  registo_window.loop();
}

public void close_cancelar_window (GWindow window) { 
  getSurface().setVisible(true);
  registo_window.setVisible(false);
} 


public void concluir_cancelar_button_click(GButton source, GEvent event) { 

  getSurface().setVisible(true);
  registo_window.setVisible(false);
  con.write("close_account " + lastNome + " " + lastPassword);
  String res = "";
    try {
       while (res.equals("")){
         Thread.sleep(300);
         res = con.read();
         println("Resposta do server: " + res);
         server_connection_label.appendText(res);
       }
    }
    catch(Exception e) {
    }
} 

public synchronized void updateJogo(String res) {

  try {
   
  StringTokenizer stk = new StringTokenizer(res, " ");

  if (stk.nextToken().equals("Pontos")) {
    MelhoresPontuacoes.clear();
    //println("Estou a ler pontos");
    int numJogadores = new Integer(stk.nextToken()).intValue();
    //println("Estou a ler pontos numjogadores" + numJogadores);
    for (int i = 0; i < numJogadores; i++) {
      String nome = new String(stk.nextToken());
      //println("Estou a ler pontos nome" + nome);
      int pontuacao = new Integer(stk.nextToken()).intValue();
      //println("Estou a ler pontos pontuacao" + pontuacao);
      MelhoresPontuacoes.put(nome, pontuacao);
      println(MelhoresPontuacoes.toString());
    }

    pontos_Window.setVisible(true);
    
  } else {

    int numJogadores = Integer.parseInt(stk.nextToken());

    ArrayList<Shooter> jogadores = new ArrayList<Shooter>();
    HashMap<String, Integer> pontos = new HashMap<String, Integer>();    

    for (int i = 0; i < numJogadores; i++) {

      String nome = new String(stk.nextToken());
      float posX = Float.parseFloat(stk.nextToken());
      float posY = Float.parseFloat(stk.nextToken());
      int col = Integer.parseInt(stk.nextToken());

      
      Shooter s = new Shooter (nome, posX, posY,col);
      jogadores.add(s);
    }

    ArrayList<Modifier> modifiers = new ArrayList<Modifier>();

    int numModifier = Integer.parseInt(stk.nextToken());

    for (int i = 0; i < numModifier; i++) {

      float posX = Float.parseFloat(stk.nextToken());
      float posY = Float.parseFloat(stk.nextToken());
      float Radius = Float.parseFloat(stk.nextToken());
      int r = Integer.parseInt(stk.nextToken());
      int g = Integer.parseInt(stk.nextToken());
      int b = Integer.parseInt(stk.nextToken());

      Modifier m = new Modifier (posX, posY, Radius,r,g,b);
      modifiers.add(m);
    }
    
    ArrayList<Bullet> bullets = new ArrayList<Bullet>();

    int numBullet = Integer.parseInt(stk.nextToken());

    for (int i = 0; i < numBullet; i++) {

      float posX = Float.parseFloat(stk.nextToken());
      float posY = Float.parseFloat(stk.nextToken());
      float Radius = Float.parseFloat(stk.nextToken());

      Bullet b = new Bullet (posX, posY, Radius);
      bullets.add(b);
    }
    
    jogo.update (jogadores, modifiers, bullets);
    
  }
  }
  catch (Exception e)
  {
    //println(e);
  }
}
