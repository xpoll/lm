package cn.blmdz.wolf.auth.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@JsonInclude(Include.NON_NULL)
public class CompiledTree implements Serializable {
	private static final long serialVersionUID = 1L;
	private String appKey;
	private String baseRole;
	private List<Node> children;

	@Data
	@JsonInclude(Include.NON_NULL)
	public static class Node implements Serializable {
		private static final long serialVersionUID = 1L;
		private String key;
		private String name;
		private String description;
		private Boolean selected;
		private List<Node> children;
	}
}
