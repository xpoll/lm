package cn.blmdz.hunt.engine.config.model;

import java.io.Serializable;
import java.util.Set;

import com.google.common.collect.Sets;

import lombok.Data;

@Data
public class Auths implements Serializable {
	private static final long serialVersionUID = 6554743011603841486L;
	
	private Set<ProtectedAuth> protectedList;
	private Set<WhiteAuth> whiteList;

	public void merge(Auths mergedAuths) {
		if ((mergedAuths.getProtectedList() != null) && (!mergedAuths.getProtectedList().isEmpty())) {
			if (this.protectedList == null) {
				this.protectedList = Sets.newHashSet();
			}
			this.protectedList.addAll(mergedAuths.getProtectedList());
		}

		if ((mergedAuths.getWhiteList() != null) && (!mergedAuths.getWhiteList().isEmpty())) {
			if (this.whiteList == null) {
				this.whiteList = Sets.newHashSet();
			}
			this.whiteList.addAll(mergedAuths.getWhiteList());
		}
	}
}
