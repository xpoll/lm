package cn.blmdz.wolf.parana.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.Map;

import lombok.Data;

@Data
public class AggNav implements Serializable {
	private static final long serialVersionUID = 8091726228902107984L;
	private Object key;
	private String name;
	private Long count;
	private Map<String, String> extra;

	public AggNav() {
	}

	@ConstructorProperties({ "key", "name", "count" })
	public AggNav(Object key, String name, Long count) {
		this.key = key;
		this.name = name;
		this.count = count;
	}
}
