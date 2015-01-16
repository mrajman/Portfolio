package client;

import java.net.*;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.io.*;

import assets.TransObject;
/**
 * 
 * @author Matt Rajman
 *
 */
public class Client extends Thread {
    final String DEFAULT_USER = "Guest";
    Socket connection;
    ArrayList<ObjectOutputStream> room;
    String name = DEFAULT_USER;
    ServerSocket receiveSocket;
    int listeningPort;

    /*
     */
    public static void main(String[] args) {

        try {
            // Display the client prompt
            new Client().clientControl();

        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
    
    /*
     * Initializes the Client class
     */
    public Client() {
        // No connection
        this.connection = null;
        
        // Create this client's chat room
        this.room = new ArrayList<ObjectOutputStream>();
        
        // Create this client's listening server socket
        this.receiveSocket = getServerSocket();
    }
    
    /* This 'beautiful' piece of code will attempt to create a server
     * socket by failing on each in-use port, until an unused port
     * has been located. Courtesy of Matthew Rajman.
     */
    private ServerSocket getServerSocket() {
        final int MAX_USERS = 50;
        for (int i = 1; i < MAX_USERS; i++) {
            try {
                /*receiveSocket.bind(new InetSocketAddress(InetAddress
                    .getLocalHost().getHostName(), (50000 + 23) + 128 * i));*/
                // store our port number
                listeningPort = (50000 + 23) + 128 * i;
                return new ServerSocket(listeningPort);
            } catch (IOException ex) {
                listeningPort = 0;
            }
        }
        // all ports are in use (MAX_USERS are connected)
        return null;
    }
    
    /*
     * Sets up a response listener for the client 
     */
    public void run() {
        while (true) {
            
            try {
                // Create and run a response listener for the client
                new ServerResponseListener(
                    this.receiveSocket.accept(), this.room, this.receiveSocket
                ).start();
                
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /*
     * Displays the prompt and handles client interaction
     */
    @SuppressWarnings("unchecked")
    public void clientControl() {
        String unparsedInput = "";
        TransObject transfer = null;

        try {
            
            BufferedReader input = new BufferedReader(
                new InputStreamReader(System.in)
            );
            ObjectOutputStream toServer = null;
            String action = ""; 
            boolean loop = true;
            while (loop) {
                // request client to input an action
                System.out.print("client> ");
                unparsedInput = input.readLine().trim();
                
                int actionInputIndex = 0;
                if ((actionInputIndex = unparsedInput.indexOf("(")) > 0) {
                    action = unparsedInput.substring(0, actionInputIndex);
                }
                transfer = parseInput(
                    action, unparsedInput.substring(actionInputIndex + 1)
                );

                // quit if action is q, Q or quit
                if ((action.equalsIgnoreCase("Q"))
                        || (action.equalsIgnoreCase("Quit"))) {
                    transfer = new TransObject(name, "quit", null);
                    toServer.writeObject(transfer);
                    loop = false;
                    
                    
                } else if (action.equals("goOnline")) {
                    // Grab the connection information
                    Map connectInfo = (Map)transfer.getData();
                    String host     = (String)connectInfo.get("host");
                    int port        = Integer.parseInt((String)connectInfo.get("port"));

                    // connect to the address
                    this.connection = new Socket(host, port);
                    
                    // open a write stream to it
                    toServer = new ObjectOutputStream(
                            this.connection.getOutputStream());
                    
                    // set the client's username and start its listener
                    this.name = transfer.getUsername();
                    
                    if(Client.activeCount() < 2) {
                        this.start();
                    }
                    
                    // set the host to the client's address
                    connectInfo.put("port", this.receiveSocket.getLocalPort());
                    connectInfo.put("host", this.receiveSocket.getInetAddress());
                    connectInfo.put("addr", this.receiveSocket.getLocalSocketAddress());
                    transfer.setData(connectInfo);
                    
                    // tell the server we have connected
                    toServer.writeObject(transfer);
                    action = "";
                    continue;
                    
                } else if (transfer.getAction().equals("sendMsg")) {
                    // skip if not in chat room
                    if(this.room.size() < 1) {
                        System.out.println("You are not in a chat.");
                        action = "";
                        continue;
                    }
                    
                    // Send the message to each user in our room
                    for (int i = 0; i < this.room.size(); i++) {
                        this.room.get(i).writeObject(transfer);
                    }
                    action = "";
                    continue;

                }
                
                Thread.sleep(100);
                
                // ignore the rest if we do not have a connection yet
                if(this.connection == null) {
                    action = "";
                    continue;
                }
                
                // If a connection to server exists, send action object
                toServer.writeObject(transfer);
                
                // sever the connection if offline command was called, 
                if (transfer.getAction().equals("goOffline")) {
                    connection = null;
                }
                
                Thread.sleep(100);
                action = "";
            }

            toServer.close();
            input.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private TransObject parseInput(String action, String unparsedInput) {
        
        int dataIndex = 0; // Used to split the string
        Map<String,String> dataMap = new HashMap<String,String>();
        String user = DEFAULT_USER;

        /* Parse the input based on the action name
         ======================================================*/
        if (action.equals("sendMsg")) {
            if ((dataIndex = unparsedInput.indexOf(")")) > 0) {
                user = this.name;
                return new TransObject(user, action, unparsedInput.substring(0, dataIndex));
            }
            
        } else if (action.equals("goOnline")) {
            if ((dataIndex = unparsedInput.indexOf(",")) > 0) {
                
                // Get the host
                dataMap.put("host", unparsedInput.substring(0, dataIndex));
                unparsedInput = unparsedInput.substring(dataIndex + 1);
                dataIndex = 0;
                
                // Get the port
                dataIndex = unparsedInput.indexOf(",");
                dataMap.put("port", unparsedInput.substring(0, dataIndex));
                unparsedInput = unparsedInput.substring(dataIndex + 1);
                dataIndex = 0;
                
                // Get the username
                dataIndex = unparsedInput.indexOf(")");
                user = unparsedInput.substring(0, dataIndex);
                dataIndex = 0;
                
            }
            
        } else if (action.equals("chatRequest")) {
            if ((dataIndex = unparsedInput.indexOf(")")) > 0) {
                user = this.name;
                // get the target user
                dataMap.put("target", unparsedInput.substring(0, dataIndex));
            }
            
        } else if (action.equals("chatAccept")) {
            if ((dataIndex = unparsedInput.indexOf(")")) > 0) {
                user = this.name;
                // get the target user
                dataMap.put("target", unparsedInput.substring(0, dataIndex));
            }
            
        } else if (action.equals("chatReject")) {
            if ((dataIndex = unparsedInput.indexOf(")")) > 0) {
                user = this.name;
                // get the target user
                dataMap.put("target", unparsedInput.substring(0, dataIndex));
            }
            
        } else if (action.equals("goOffline")) {
            if ((dataIndex = unparsedInput.indexOf(")")) > 0) {
                user = this.name;
                this.name = DEFAULT_USER;
            }
        }

        return new TransObject(user, action, dataMap);
    }

}

class ServerResponseListener extends Thread {
    Socket connection;
    ArrayList<ObjectOutputStream> room;
    ServerSocket receiveSocket;

    public ServerResponseListener(Socket initSock,
            ArrayList<ObjectOutputStream> room, ServerSocket receiveSocket) {
        this.connection    = initSock;
        this.room          = room;
        this.receiveSocket = receiveSocket;
    }

    // print out messages received
    public void run() {

        try {
            TransObject response;

            ObjectInputStream fromServer = new ObjectInputStream(
                    connection.getInputStream());

            // parse response from server if there is one
            while ((response = (TransObject) fromServer.readObject()) != null) {

                if ((response.getAction().equals("sendMsg") || response
                        .getAction().equals("serverMsg"))
                        && response.getData() != null) {
                    System.out.println(response.toString());
                    
                } else if (response.getAction().equals("updateRoom")) {
                    // Empty the room
                    this.room.clear();
                    ArrayList<InetSocketAddress> socketList = (ArrayList<InetSocketAddress>) response.getData();
                    
                    // Fill the room with write streams
                    for (int i = 0; i < socketList.size(); i++) {
                        // connect to each client address and store the write stream
                        InetSocketAddress currentAddress = socketList.get(i);
                        Socket temp = new Socket();
                        temp.connect(currentAddress);
                        this.room.add(new ObjectOutputStream(temp.getOutputStream()));
                    }
                    
                } else if (response.getAction().equals("removeUser")) {
                    this.room.remove(response.getData());
                    
                } else if (response.getAction().equals("kill")) {
                    System.exit(0);
                }
            }
        } catch (Exception e) {
            
        }
    }
}