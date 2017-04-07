package cn.blmdz.rabbit.web.design.modal;

import java.util.List;
import java.util.Map;

import cn.blmdz.hunt.design.medol.Page;
import cn.blmdz.hunt.design.medol.Site;
import lombok.Getter;
import lombok.Setter;

/**
 * Author:cp
 * Created on 11/5/15.
 */
public class SiteContent extends BaseDesignContent {
    @Setter
    @Getter
    private Site site;
    @Setter
    @Getter
    private List<Page> pages;
    @Setter
    @Getter
    private Map<Long, Map<String, String>> pageParts;
    @Setter
    @Getter
    private Map<String, String> siteParts;
    @Setter
    @Getter
    private Map<String, String> globalParts;
}
