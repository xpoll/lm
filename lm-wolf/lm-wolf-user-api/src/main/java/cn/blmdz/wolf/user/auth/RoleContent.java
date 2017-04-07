package cn.blmdz.wolf.user.auth;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

@Data
public class RoleContent implements Serializable {
	private static final long serialVersionUID = 1L;
	private List<Role> roles;
	private List<Role> dynamicRoles;
}