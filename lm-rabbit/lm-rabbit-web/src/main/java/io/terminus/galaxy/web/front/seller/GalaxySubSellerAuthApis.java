package io.terminus.galaxy.web.front.seller;

import com.google.api.client.util.Sets;
import com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.MainSellerRoleReadService;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.auth.core.AclLoader;
import io.terminus.parana.auth.model.Acl;
import io.terminus.parana.auth.model.CompiledTree;
import io.terminus.parana.auth.model.ParanaThreadVars;
import io.terminus.parana.auth.model.TreeNode;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.common.utils.Iters;
import io.terminus.parana.common.utils.Strs;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/seller/sub/tree")
public class GalaxySubSellerAuthApis {

    @Autowired
    private AclLoader aclLoader;

    @Autowired
    private SellerReadService sellerReadService;

    @Autowired
    private MainSellerRoleReadService mainSellerRoleReadService;

    @RequestMapping(value = "", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public CompiledTree getTree(@RequestParam String role) {
        try {
            Set<String> allows = getAllows();
            Acl acl = aclLoader.getAcl(ParanaThreadVars.getApp());
            Map<String, TreeNode> tree = Iters.nullToEmpty(acl.getTrees()).get(role);
            if (tree == null) {
                log.warn("auth tree not found, role={}", role);
                throw new JsonResponseException("auth.tree.not.found");
            }
            CompiledTree ct = new CompiledTree();
            ct.setAppKey(ParanaThreadVars.getAppKey());
            ct.setBaseRole(role);
            ct.setChildren(compileTree(tree, allows));
            return ct;
        } catch (Exception e) {
            Throwables.propagateIfInstanceOf(e, JsonResponseException.class);
            log.error("get auth tree failed, role={}, cause:{}",
                    role, Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("auth.tree.find.fail");
        }
    }

    private Set<String> getAllows() {
        ParanaUser user = UserUtil.getCurrentUser();
        Set<String> allows = getAllows(user.getPresentUserId());
        if (allows == null) {
            return Sets.newHashSet();
        }
        return allows;
    }

    private Set<String> getAllows(Long userId) {
        val rSeller = sellerReadService.findSellerByUserId(userId);
        if (!rSeller.isSuccess()) {
            log.warn("find seller failed, userId={}, error={}", userId, rSeller.getError());
            return null;
        }
        Seller seller = rSeller.getResult().orNull();
        if (seller == null) {
            log.warn("seller not found, userId={}", userId);
            return null;
        }
        if (!seller.isActive()) {
            log.warn("seller not active, userId={}", userId);
            return null;
        }
        val map = seller.getExtra();
        if (map == null) {
            log.warn("seller has no role, userId={}", userId);
            return null;
        }
        Long roleId = Strs.parseLong(map.get("roleId")).orNull();
        if (roleId == null) {
            log.warn("seller has no role, userId={}", userId);
            return null;
        }
        val rRole = mainSellerRoleReadService.findById(roleId);
        if (!rRole.isSuccess()) {
            log.warn("find seller role failed, userId={}, roleId={}", userId, roleId);
            return null;
        }
        MainSellerRole role = rRole.getResult();
        if (!role.isActive()) {
            log.warn("seller role not active, userId={}, roleId={}", userId, roleId);
            return null;
        }
        Set<String> allows = Sets.newHashSet();
        allows.addAll(Iters.nullToEmpty(role.getAllow()));
        return allows;
    }

    private List<CompiledTree.Node> compileTree(Map<String, TreeNode> tree, Set<String> allows) {
        if (tree == null) {
            return new ArrayList<>();
        }
        List<CompiledTree.Node> result = new ArrayList<>();
        for (String key : tree.keySet()) {
            if (!allows.contains(key)) {
                continue;
            }
            TreeNode raw = tree.get(key);
            CompiledTree.Node n = new CompiledTree.Node();
            result.add(n);
            n.setKey(key);
            n.setName(raw.getName());
            n.setDescription(raw.getDescription());
            n.setSelected(Boolean.FALSE);
            n.setChildren(compileTree(raw.getChildren(), allows));
        }
        return result;
    }
}
