package cn.blmdz.hunt.engine;

import java.util.List;

import cn.blmdz.hunt.engine.model.App;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public abstract class AbstractSetting {
	private String rootPath;
	private List<App> apps;
	private String registryId;
	private boolean devMode = false;
	private String locale;
	private boolean clearInjectNestedContext = false;
}