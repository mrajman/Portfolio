package server;

import java.net.*;
import java.io.*;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;

import assets.*;

public class Server {

    private static ServerSocket serverSocket;

    public static void main(String[] args) {
        int port = 50000 + 23 + 128 * 0;
        new Server(port).runServer(port);
    }

    // construct the server, port is passed in
    public Server(int serverPort) {
        try {
            this.serverSocket = new ServerSocket(serverPort);

            /*this.serverSocket.bind(new InetSocketAddress(InetAddress
                    .getLocalHost().getHostName(), serverPort));*/
            System.out.println("Server Started  on "
                    + InetAddress.getLocalHost().getHostName()
                    + " - Ready for Connections");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // runs the server on a given port, waiting for input
    public static void runServer(int serverPort) {
        ClientHandle clientThread;
        Socket client;
        Map<String, ClientHandle> listOfClients = new HashMap<String, ClientHandle>();
        ArrayList<ArrayList<String>> listOfRooms = new ArrayList<ArrayList<String>>();
        try {

            while (true) {
                // accept a socket from the server
                client = serverSocket.accept();

                System.out.println("Server: User is connected on port "
                        + serverPort + "to address:"
                        + client.getRemoteSocketAddress().toString());
                System.out.println(listOfClients.toString());
                ObjectInputStream ois = (new ObjectInputStream(
                        client.getInputStream()));

                clientThread = new ClientHandle(
                    client, listOfClients, listOfRooms, ois
                );
                clientThread.start();
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class ClientHandle extends Thread {
    String identity;
    ObjectOutputStream outputToClient;
    ObjectInputStream fromClient;
    Socket socket;
    Map<String, ClientHandle> onlineList; // reference to main's user list
    InetSocketAddress addressInfo;
    ArrayList<ArrayList<String>> rooms;

    /*
     * Constructor, inits require references
     */
    ClientHandle(Socket openConnection,
            Map<String, ClientHandle> listOfClients,
            ArrayList<ArrayList<String>> listOfRooms,
            ObjectInputStream ois) {
        identity = "Guest";
        socket = openConnection;
        onlineList = listOfClients; // reference
        rooms = listOfRooms; // reference
        fromClient = ois;

        try {
            // fromClient = new ObjectInputStream(socket.getInputStream());
        } catch (Exception e) {
            System.out.println("Error creating streams: " + e.getMessage());
        }
    }

    /*
     * Handles incoming requests from this socket
     */
    public void run() {
        String username = "Guest";
        TransObject incomingMsg = null;
        TransObject outgoingMsg;
        try {
            // read the transfer object in from the client thread
            while ((incomingMsg = (TransObject) this.fromClient.readObject()) != null) {
                outgoingMsg = null;
                // if quit or q, break out of the receive loop
                if ((incomingMsg.getAction().equalsIgnoreCase("quit"))
                        || (incomingMsg.getAction().equalsIgnoreCase("q"))) {
                    break;

                // if goOnline, connect the client
                } else if ((incomingMsg.getAction()
                        .equalsIgnoreCase("goOnline"))) {
                    username = incomingMsg.getUsername();

                    // save the connection information
                    this.addressInfo = new InetSocketAddress(
                        this.socket.getInetAddress(),
                        ((int)((Map)incomingMsg.getData()).get("port"))
                    );
                    
                    try {
                        // Attempt to create a connection to the client
                        Socket output = new Socket();
                        output.connect(this.addressInfo);
                        outputToClient = new ObjectOutputStream(output.getOutputStream());
                    } catch (Exception e) {
                        System.out.println("Error connecting to client: " + e.getMessage());
                        username = "Guest";
                        this.addressInfo = null;
                        continue;
                    }
                    
                    outgoingMsg = this.goOnline(username);
                    
                // require online user for these commands
                } else if (isOnline(username)) {

                    // if goOffline, disconnect the client
                    if ((incomingMsg.getAction().equalsIgnoreCase("goOffline"))) {
                        this.goOffline(username);
                        username = "Guest";
                        break;

                    // if sendMsg, send a message
                    } else if ((incomingMsg.getAction()
                            .equalsIgnoreCase("sendMsg"))) {
                        outgoingMsg = this.sendMsgToRoom(incomingMsg);

                    // if chatRequest, requests chat with user
                    } else if ((incomingMsg.getAction()
                            .equalsIgnoreCase("chatRequest"))) {
                        this.chatRequest(username,
                                (String) ((Map)incomingMsg.getData()).get("target"));

                    // if chatAccept, requests chat with user
                    } else if ((incomingMsg.getAction()
                            .equalsIgnoreCase("chatAccept"))) {
                        this.chatAccept(username,
                                (String) ((Map)incomingMsg.getData()).get("target"));

                    // if chatReject, requests chat with user
                    } else if ((incomingMsg.getAction()
                            .equalsIgnoreCase("chatReject"))) {
                        this.chatReject(username,
                                (String) ((Map)incomingMsg.getData()).get("target"));
                        
                    // if chatReject, requests chat with user
                    } else if ((incomingMsg.getAction()
                            .equalsIgnoreCase("quitChat"))) {
                        this.removeUserFromRooms(username);

                        // default, online
                    } else {
                        outgoingMsg = new TransObject("Server",
                                "Not a valid command.");
                    }

                // default
                } else {
                    outgoingMsg = new TransObject("Server",
                            "Not a valid command.");
                }

                if (outgoingMsg != null) {
                    this.outputToClient.writeObject(outgoingMsg);
                }

            }

            // the client has closed connection
            fromClient.close();
            outputToClient.close();
            socket.close();
            System.out.println("Client: " + this.identity
                    + " connection closed");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void displayOnlineList() {
        System.out.println(this.onlineList.toString());
    }
    
    /*
     * isOnline : Checks if a user is logged in
     */
    private boolean isOnline(String user) {
        return this.onlineList.containsKey(user);
    }

    /*
     * Returns the given user's room
     */
    private ArrayList<String> getUserRoom(String user) {
        for (int k = 0; k < this.rooms.size(); k++) {
            if (this.rooms.get(k).contains(user)) {
                return this.rooms.get(k);
            }
        }
        // single user room (not in chat)
        ArrayList<String> singleUser = new ArrayList<String>();
        singleUser.add(user);
        return singleUser;
    }

    /*
     * Returns the given user's room removes it from the main list
     */
    private ArrayList<String> popUserRoom(String user) {
        ArrayList<String> temp;
        for (int k = 0; k < this.rooms.size(); k++) {
            if ((temp = this.rooms.get(k)).contains(user)) {
                this.rooms.remove(k);
                return temp;
            }
        }
        // single user room (not in chat)
        ArrayList<String> singleUser = new ArrayList<String>();
        singleUser.add(user);
        return singleUser;
    }

    /*
     * Combines list of user rooms
     */
    private ArrayList<String> combineUserRooms(String userOne, String userTwo) {

        // get their rooms, remove them from the list
        ArrayList roomOne = this.popUserRoom(userOne);
        ArrayList roomTwo = this.popUserRoom(userTwo);

        // combine them, return the new room
        ArrayList<String> newRoom = new ArrayList<String>(roomOne);
        newRoom.addAll(roomTwo);
        return newRoom;
    }

    /*
     * Checks if a user is in a chat room
     */
    private boolean isInChat(String user) {
        return (this.getUserRoom(user).size() > 1);
    }

    /*
     * Logs a user in to the server
     */
    private TransObject goOnline(String connectingUser) {

        if (this.getIdentity() != "Guest") {
            return new TransObject("Server", this.getIdentity()
                    + " is already logged in.");
        } else if (isOnline(connectingUser)) {
            return new TransObject("Server", "User is already logged in.");
        }
        this.setIdentity(connectingUser);
        this.onlineList.put(connectingUser, this);
        return new TransObject("Server", "User " + connectingUser
                + " has been logged in.");
    }

    /* 
     * Update the room list and inform everyone
     * that the room list has been updated. It removes the room
     * owner's address, before sending. - Matt Rajman
     */
    private void updateRoom(String user) {
        this.updateRoom(user, false);
    }
    private void updateRoom(String user, boolean removeRoom) {
        ArrayList<String> requestingUserRoom = this.getUserRoom(user);
        ArrayList<InetSocketAddress> roomlist = new ArrayList<InetSocketAddress>();
        ArrayList<InetSocketAddress> emptyRoomList = new ArrayList<InetSocketAddress>();
        
        // remove the user is needed
        if(removeRoom) {
            requestingUserRoom.remove(user);
        }
        for (int i = 0; i < requestingUserRoom.size(); i++) {
            // save all requesting user room addresses
            roomlist.add(
                this.onlineList.get(requestingUserRoom.get(i)).addressInfo
            );
        }
         
        for (int i = 0; i < requestingUserRoom.size(); i++) {
            InetSocketAddress currentRoomOwner = null;
            // Temporarily remove the currentRoom owner's address
            currentRoomOwner = roomlist.remove(i);

            this.onlineList.get(requestingUserRoom.get(i)).sendMsg(
                new TransObject("Server", "updateRoom", roomlist)
            );
            
            // Add the currentRoom owner's address back
            roomlist.add(i, currentRoomOwner);
        }
        
        // Kick user out of the room if removing
        if(removeRoom) {
            // send the user an empty room list
            this.onlineList.get(user).sendMsg(
                new TransObject("Server", "updateRoom", emptyRoomList)
            );
        }
        
    }
    
    /*
     * Removes a user from all rooms
     */
    private void removeUserFromRooms(String user) {
        this.updateRoom(user, true);
        for (int k = 0; k < this.rooms.size(); k++) {
            if (this.rooms.get(k).contains(user)) {
                this.rooms.get(k).remove(user);
            }
            // if the room is not chat sized, clear it
            if(this.rooms.get(k).size() < 2) {
                this.rooms.remove(k);
            }
        }
    }

    /*
     * Logs a user out of the server
     */
    private void goOffline(String disconnectingUser) {

        this.setIdentity("Guest");
        this.removeUserFromRooms(disconnectingUser);
        this.onlineList.remove(disconnectingUser);
        this.sendMsg(new TransObject("Server", "User " + disconnectingUser
                + " has been logged out."));
    }

    /*
     * Sends a chat request to a user
     */
    private synchronized void chatRequest(String from, String to) {
        if(this.isOnline(from) && this.isInChat(to)) {
            TransObject msg = new TransObject("Server",
                    "User " + to + " is already in a chat.");
            this.onlineList.get(from).sendMsg(msg);
            return;
        }
        if (this.isOnline(from) && this.isOnline(to)) {
            TransObject msg = new TransObject("Server",
                    "Accept chat request from " + from + "?");
            this.onlineList.get(to).sendMsg(msg);
            return;
        }
        TransObject msg = new TransObject("Server", "User is not online.");
        this.onlineList.get(from).sendMsg(msg);
    }

    /*
     * Accepts a chat request from a user
     */
    private synchronized void chatAccept(String from, String to) {
        if(this.isOnline(from) && this.isInChat(to)) {
            TransObject msg = new TransObject("Server",
                    "User " + to + " is already in a chat.");
            this.onlineList.get(from).sendMsg(msg);
            return;
        }
        
        if (!this.isOnline(to)) {
            TransObject msg = new TransObject("Server", "User " + to
                    + " is no longer online.");
            this.onlineList.get(from).sendMsg(msg);
            return;
        }

        // add the user, notify sender of update
        this.rooms.add(this.combineUserRooms(from, to));
        TransObject msg = new TransObject("Server", "User " + from
                + " accepted your chat request.");
        this.onlineList.get(to).sendMsg(msg);
        System.out.println(this.rooms.toString());

       this.updateRoom(from);
    }

    /*
     * Rejects a chat request from a user
     */
    private synchronized void chatReject(String from, String to) {
        if (this.isOnline(from) && isOnline(to)) {
            TransObject msg = new TransObject("Server", "User " + from
                    + " rejected your chat request.");
            this.onlineList.get(to).sendMsg(msg);
        }
    }

    /*
     * sendMsg : Sends a message if a user is online
     */
    private synchronized TransObject sendMsgToRoom(TransObject msg) {
        String from = msg.getUsername();
        if (!isInChat(from)) {
            return new TransObject("Server", "User is not in a chat.");
        }
        String name;
        ClientHandle handle;
        ArrayList<String> room = this.getUserRoom(from);

        for (int u = 0; u < room.size(); u++) {
            name = room.get(u);
            // skip current user
            if (name.equals(from)) {
                continue;
            }
            // try to send message
            handle = this.onlineList.get(name);
            if (!handle.sendMsg(msg)) {
                this.onlineList.remove(name);
            }
        }
        return new TransObject("", null);
    }

    private boolean sendMsg(TransObject msg) {
        if (!this.socket.isConnected()) {
            // this.socket.close();
            return false;
        }
        // write the message to the stream
        try {
            this.outputToClient.writeObject(msg);
        }
        // if an error occurs, do not abort just inform the user
        catch (IOException e) {
            System.out.println(e.toString());
        }
        return true;
    }

    private void setIdentity(String id) {
        this.identity = id;
    }

    public String getIdentity() {
        return this.identity;
    }
}