package cn.blmdz.rabbit.web.design.controller.ext;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.design.controller.ext.ComponentsHook;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.hunt.design.util.PagePartPath;
import cn.blmdz.hunt.design.util.PagePartScope;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.web.design.modal.EcpComponentCategory;
import cn.blmdz.rabbit.web.design.service.EcpSiteService;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/16
 */
@Component
public class EcpComponentsHookImpl implements ComponentsHook {
    @Autowired(required = false)
    private SiteService siteService;
    @Autowired
    private EcpSiteService ecpSiteService;

    @Override
    public Map<String, String> listCategories(String mode, String pagePath) {
        List<EcpComponentCategory> categoryList = Lists.newArrayList();
        categoryList.add(EcpComponentCategory.COMMON);

        if (UserUtil.getCurrentUser().getType() == UserType.ADMIN.value()){
            categoryList.add(EcpComponentCategory.SHOP);
        }else {
            PagePartPath pagePartPath = new PagePartPath(pagePath);
            PagePartScope scope = PagePartScope.fromString(pagePartPath.getScope());
            if (scope == PagePartScope.PAGE) {
                Long pageId = Long.valueOf(pagePartPath.getKey());
                Site site = siteService.findByPageId(pageId);
                boolean isShopLayout = ecpSiteService.isShopLayout(site);
                if (isShopLayout) {
                    categoryList.add(EcpComponentCategory.SHOP);
                }
            }
        }
        
        if (!Strings.isNullOrEmpty(mode)) {
            categoryList.add(EcpComponentCategory.ITEM);
        }
        if (UserUtil.getCurrentUser().getType() == UserType.ADMIN.value()) {
            categoryList.add(EcpComponentCategory.OFFICIAL);
        }
        Map<String, String> result = Maps.newHashMap();
        for (EcpComponentCategory category : categoryList) {
            result.put(category.name(), category.getName());
        }
        return result;
    }
}
