package cn.blmdz.wolf.category.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

@Data
public class BackCategory implements Serializable {
	private static final long serialVersionUID = -8511655074628678510L;
	private Long id;
	private Long pid;
	private String name;
	private Integer level;
	private Integer status;
	private Boolean hasChildren;
	private Boolean hasSpu;
	private String outerId;
	private Date createdAt;
	private Date updatedAt;
}
