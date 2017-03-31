package cn.blmdz.hunt.engine.service;

import java.util.List;

import cn.blmdz.hunt.engine.config.model.BackConfig;
import cn.blmdz.hunt.engine.config.model.FrontConfig;
import cn.blmdz.hunt.engine.config.model.Render;
import cn.blmdz.hunt.engine.model.App;
import cn.blmdz.hunt.engine.model.AppWithConfigInfo;

public interface ConfigService {
	List<AppWithConfigInfo> listAllAppWithConfigInfo();

	App getApp(String appKey);

	FrontConfig getFrontConfig(String appKey);

	FrontConfig getDefaultFrontConfig();

	BackConfig getBackConfig(String appKey);

	BackConfig getDefaultBackConfig();

	List<Render.Layout> listAllLayouts();

	List<Render.Layout> listLayouts(String paramString);

	List<Render.Layout> listLayouts(String paramString1, String paramString2);

	Render.Layout findLayout(String paramString1, String paramString2);
}