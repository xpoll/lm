package cn.blmdz.wolf.user.auth;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import lombok.Data;

@Data
public class Role implements Serializable {
	private static final long serialVersionUID = 0L;
	private String base;
	private int type;
	private Map<String, String> context;
	private List<String> treeNodeSelection;

	public static Role createStatic(String roleText) {
		Role r = new Role();
		r.setBase(roleText);
		r.setType(1);
		r.setContext(Maps.<String, String>newHashMap());
		r.setTreeNodeSelection(null);
		return r;
	}

	public static Role createDynamic(String roleText, List<String> nodes) {
		Role r = new Role();
		r.setBase(roleText);
		r.setType(2);
		r.setContext(Maps.<String, String>newHashMap());
		r.setTreeNodeSelection(Lists.newArrayList(nodes));
		return r;
	}


}