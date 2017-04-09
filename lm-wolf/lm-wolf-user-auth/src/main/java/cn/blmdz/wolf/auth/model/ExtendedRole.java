package cn.blmdz.wolf.auth.model;

import com.google.common.collect.Multimap;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
public class ExtendedRole implements Serializable {
	private static final long serialVersionUID = 0L;
	private String base;
	private String level;
	private List<Long> levelContext;
	private Multimap<String, String> otherContext;
}
