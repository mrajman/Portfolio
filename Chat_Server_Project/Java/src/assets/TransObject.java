package assets;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;

/**
 * 
 * @author Stephan
 */
public class TransObject implements Serializable {
	private static final long serialVersionUID = 1L;
	private String username;
	private String action;
	private Object data;
	
	public TransObject(TransObject b) {
		this.username = b.username;
		this.action = b.action;
		this.data = b.data;
	}
	
	public TransObject(String user, Object data) {
		this.username = user;
		this.action = "serverMsg";
		this.data = data;
	}
	
	public TransObject(String user, String action, Object data) {
		this.username = user;
		this.action = action;
		this.data = data;
	}
	
	public void setAction(String newAction) {
		 this.action = newAction;
	}
	
	public String getAction() {
		return this.action;
	}
	
	public void setUsername(String newName) {
		this.username = newName;
	}
	
	public String getUsername() {
		return this.username;
	}
	
	public Object getData() {
		return this.data;
	}
	
	public void setData(Object newData) {
		this.data = newData;
	}

	@Override
	public boolean equals(Object b) {
		return this.username == ((TransObject)b).username
				&& this.action == ((TransObject)b).action
				&& this.data == ((TransObject)b).data;
	}
	
	@Override
	public String toString() {
		return this.username.toString() + ":" + this.data.toString();
	}

}