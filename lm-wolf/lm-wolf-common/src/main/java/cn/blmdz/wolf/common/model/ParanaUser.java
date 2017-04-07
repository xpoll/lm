package cn.blmdz.wolf.common.model;

import java.util.List;

import cn.blmdz.home.common.model.BaseUser;
import lombok.Data;

@Data
public class ParanaUser implements BaseUser {
	private static final long serialVersionUID = -2961193418926377287L;
	private Long id;
	private String name;
	private Integer type;
	private Long shopId;
	private List<String> roles;
	private Long presentUserId;

	@Override
	public String getTypeName() {
		return null;
	}
}
